package zio.intellij.project.npw

import com.intellij.ide.wizard.GeneratorNewProjectWizardBuilderAdapter
import zio.intellij.project.npw.template.wizard.ZioProjectWizard

final class ZioModuleBuilderAdapter extends GeneratorNewProjectWizardBuilderAdapter(ZioProjectWizard)
