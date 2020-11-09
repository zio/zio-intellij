package zio.intellij.ui

import java.awt.BorderLayout
import java.awt.datatransfer.StringSelection

import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.icons.AllIcons
import com.intellij.notification.{NotificationGroup, NotificationGroupManager}
import com.intellij.openapi.actionSystem._
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.ide.CopyPasteManager
import com.intellij.openapi.project.{DumbAware, DumbAwareAction, Project}
import com.intellij.openapi.ui.{MessageType, Splitter}
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.wm.{IdeFocusManager, ToolWindowId}
import com.intellij.ui._
import com.intellij.ui.components.JBList
import com.intellij.util.PlatformIcons
import com.intellij.util.ui.UIUtil
import javax.swing._
import javax.swing.event.{DocumentEvent, ListSelectionEvent}
import zio.intellij.icons
import zio.intellij.ide.FiberDumpToFileExporter
import zio.intellij.ui.FiberDumpPanel.getFiberStatusIcon
import zio.intellij.utils.jdi.fiber.model.{FiberInfo, FiberStatus}

/**
 * This class was copied from the intellij-community project and modified
 *
 * https://github.com/JetBrains/intellij-community/blob/48036571856fa3482e394e9bec2a33f9876871b0/java/java-impl/src/com/intellij/unscramble/ThreadDumpPanel.java#L54
 */
class FiberDumpPanel(
  project: Project,
  consoleView: ConsoleView,
  toolbarActions: DefaultActionGroup,
  private var fiberDump: List[FiberInfo]
) extends JPanel(new BorderLayout())
    with DataProvider {

  private val filterField = new SearchTextField
  filterField.addDocumentListener(new DocumentAdapter() {
    override protected def textChanged(e: DocumentEvent): Unit = updateFiberList()
  })
  private val filterPanel = new JPanel(new BorderLayout())
  filterPanel.add(new JLabel("Filter:"), BorderLayout.WEST)
  filterPanel.add(filterField)
  filterPanel.setVisible(false)

  private val fibers = new JBList[FiberInfo](new DefaultListModel[FiberInfo])
  fibers.setCellRenderer(new FiberListCellRenderer)
  fibers.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  fibers.addListSelectionListener { (_: ListSelectionEvent) =>
    val index = fibers.getSelectedIndex
    if (index >= 0) {
      val selection = fibers.getModel.getElementAt(index)
      FiberDumpPanel.printTrace(consoleView, Option(selection))
    } else FiberDumpPanel.printTrace(consoleView, None)
    fibers.repaint()
  }

  toolbarActions.addAction(new FilterAction)
  toolbarActions.addAction(new CopyFiberDumpToClipboardAction)
  toolbarActions.addAction(new SortFibersAction)
  toolbarActions.addAction(ActionManager.getInstance.getAction(IdeActions.ACTION_EXPORT_TO_TEXT_FILE))

  private val toolbar = ActionManager.getInstance.createActionToolbar("FiberDump", toolbarActions, false)
  toolbar.setTargetComponent(consoleView.getComponent)
  add(toolbar.getComponent, BorderLayout.WEST)

  private val leftPanel = new JPanel(new BorderLayout())
  leftPanel.add(filterPanel, BorderLayout.NORTH)
  leftPanel.add(ScrollPaneFactory.createScrollPane(fibers, SideBorder.LEFT | SideBorder.RIGHT), BorderLayout.CENTER)

  val splitter = new Splitter(false, 0.3f)
  splitter.setFirstComponent(leftPanel)
  splitter.setSecondComponent(consoleView.getComponent)
  add(splitter, BorderLayout.CENTER)

  updateFiberList()

  override def getData(dataId: String): AnyRef =
    if (PlatformDataKeys.EXPORTER_TO_TEXT_FILE.is(dataId)) new FiberDumpToFileExporter(project, fiberDump)
    else null

  private def updateFiberList(): Unit = {
    val text      = if (filterPanel.isVisible) filterField.getText else ""
    val selection = fibers.getSelectedValue
    val model     = fibers.getModel.asInstanceOf[DefaultListModel[FiberInfo]]
    model.clear()

    val selectedIndex =
      if (text.isEmpty) {
        fiberDump.foreach(model.addElement)
        0
      } else
        fiberDump.zipWithIndex.foldLeft(0) {
          case (acc, (dump, dumpIdx)) =>
            val renderedFiberInfo = FiberDumpRenderer.renderFiberInfo(dump)
            if (StringUtil.containsIgnoreCase(renderedFiberInfo, text)) {
              model.addElement(dump)
              if (selection == dump) dumpIdx else acc
            } else acc
        }

    if (!model.isEmpty)
      fibers.setSelectedIndex(selectedIndex)
    fibers.revalidate()
    fibers.repaint()
  }

  private class FilterAction
      extends ToggleAction("Filter", "Show only fiber traces containing a specific string", AllIcons.General.Filter)
      with DumbAware {
    override def isSelected(event: AnActionEvent): Boolean = filterPanel.isVisible

    override def setSelected(event: AnActionEvent, state: Boolean): Unit = {
      filterPanel.setVisible(state)
      if (state) {
        IdeFocusManager.getInstance(AnAction.getEventProject(event)).requestFocus(filterField, true)
        filterField.selectText()
      }
      updateFiberList()
    }
  }

  private class CopyFiberDumpToClipboardAction
      extends DumbAwareAction("Copy to Clipboard", "Copy whole thread dump to clipboard", PlatformIcons.COPY_ICON) {
    private val GROUP = NotificationGroupManager.getInstance().getNotificationGroup("Analyze fiber dump")

    override def actionPerformed(e: AnActionEvent): Unit = {
      val separator = "\n\n"
      val buf       = new StringBuilder
      buf.append("Full fiber dump").append(separator)
      fiberDump.foreach(info => buf.append(FiberDumpRenderer.renderFiberInfo(info)).append(separator))
      CopyPasteManager.getInstance().setContents(new StringSelection(buf.toString()))

      GROUP.createNotification("Full fiber dump was successfully copied to clipboard", MessageType.INFO).notify(project)
    }
  }

  private class SortFibersAction extends DumbAwareAction {
    private val BY_STATUS: Ordering[FiberInfo] = Ordering[FiberStatus].on[FiberInfo](_.status)
    private val BY_ID: Ordering[FiberInfo]     = Ordering[Long].on[FiberInfo](_.id.seqNumber)
    private val BY_NAME: Ordering[FiberInfo]   = Ordering[Option[String]].on[FiberInfo](_.name)

    private var ORDERING: Ordering[FiberInfo] = BY_STATUS

    override def actionPerformed(event: AnActionEvent): Unit = {
      fiberDump = fiberDump.sorted(ORDERING)
      updateFiberList()
      toggleOrdering()
      update(event)
    }

    override def update(event: AnActionEvent): Unit = {
      event.getPresentation.setIcon(getIcon)
      event.getPresentation.setText(getLabel)
    }

    private def toggleOrdering(): Unit =
      ORDERING = ORDERING match {
        case BY_STATUS => BY_ID
        case BY_ID     => BY_NAME
        case _         => BY_STATUS
      }

    private def getIcon: Icon =
      ORDERING match {
        case BY_STATUS => AllIcons.ObjectBrowser.SortByType
        case BY_ID     => icons.SortByIdIcon
        case _         => AllIcons.ObjectBrowser.Sorted
      }

    private def getLabel: String =
      ORDERING match {
        case BY_STATUS => FiberDumpPanel.SORT_BY_STATUS_LABEL
        case BY_ID     => FiberDumpPanel.SORT_BY_ID_LABEL
        case _         => FiberDumpPanel.SORT_BY_NAME_LABEL
      }
  }

}

private class FiberListCellRenderer extends ColoredListCellRenderer[FiberInfo] {

  override def customizeCellRenderer(
    list: JList[_ <: FiberInfo],
    fiber: FiberInfo,
    index: Int,
    selected: Boolean,
    hasFocus: Boolean
  ): Unit = {
    setIcon(getFiberStatusIcon(fiber))
    if (!selected) setBackground(UIUtil.getListBackground)
    append(s"${fiber.renderName} (${fiber.status.name})")
  }
}

object FiberDumpPanel {
  val SORT_BY_STATUS_LABEL = "Sort fibers by status"
  val SORT_BY_NAME_LABEL   = "Sort fibers by name"
  val SORT_BY_ID_LABEL     = "Sort fibers by id"

  def getFiberStatusIcon(fiber: FiberInfo): Icon =
    fiber.status match {
      case _: FiberStatus.Running   => AllIcons.Actions.Execute
      case _: FiberStatus.Suspended => AllIcons.Actions.Pause
      case _: FiberStatus.Finishing => AllIcons.Actions.Exit
      case FiberStatus.Done         => AllIcons.Actions.Cancel
    }

  def printTrace(consoleView: ConsoleView, fiberInfo: Option[FiberInfo]): Unit = {
    ApplicationManager.getApplication.assertIsDispatchThread()
    val consoleText = consoleView.asInstanceOf[ConsoleViewImpl].getText
    val text        = fiberInfo.fold("")(FiberDumpRenderer.renderFiberInfo)
    if (text != consoleText) {
      consoleView.clear()
      consoleView.print(text, ConsoleViewContentType.ERROR_OUTPUT)
      consoleView.scrollTo(0)
    }
  }

}
