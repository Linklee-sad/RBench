source("setup.R", local = TRUE)
library(shiny)
# Transport ceiling for a batch request. The import panel still validates each
# individual file against the selected limit of up to 1000 MB.
options(shiny.maxRequestSize = 4 * 1024^3)
source("R/import.R")
source("R/project.R")
source("R/home.R")
source("R/i18n.R")
source("R/ai.R")
source("R/preview.R")
source("R/workbench.R")
source("R/plotting.R")
source("R/function_plotter.R")
source("R/directory.R")
source("R/plot_editor.R")
source("R/model_evaluation.R")
source("R/algorithm_tutorials.R")
source("R/distributions.R")
source("R/teaching.R")
source("R/regression.R")
source("R/logistic_regression.R")
source("R/advanced_regression.R")
source("R/random_forest_tutorial.R")
source("R/random_forest.R")
source("R/decision_tree.R")
source("R/model_comparison.R")
source("R/svm.R")
source("R/pca.R")
source("R/kmeans.R")
source("R/timeseries.R")

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f5f7fb;color:#172b4d}.container-fluid{max-width:1400px;margin:auto}.well{background:white;border:1px solid #e3e8ef;border-radius:12px}h2{font-weight:700}.btn-primary{background:#2563eb;border-color:#2563eb}.dataset-status-bar{background:#eaf2ff;border:1px solid #bdd3f8;border-radius:12px;padding:12px 16px 2px;margin:10px 0 16px}.dataset-status-text{padding-top:30px;font-weight:600;color:#174a8b}.dataset-status-bar .btn{margin-top:25px}.language-switch{position:absolute;right:22px;top:14px;z-index:2000}.language-switch .btn{background:white;border:1px solid #9db7df;box-shadow:0 2px 8px rgba(23,74,139,.16);font-weight:600}.ai-parameter-panel{border:1px solid #9fc2f5;border-radius:12px;background:#f8fbff;margin:14px 0;padding:0 14px 14px;box-shadow:0 4px 14px rgba(37,99,235,.10)}.ai-parameter-panel summary{display:flex;align-items:center;gap:9px;margin:0 -14px 12px;padding:13px 14px;cursor:pointer;font-weight:800;color:#174a8b;background:#eaf3ff;border-radius:12px;list-style:none}.ai-parameter-panel summary::-webkit-details-marker{display:none}.ai-parameter-badge{margin-left:auto;background:#2563eb;color:#fff;border-radius:999px;padding:2px 8px;font-size:11px;letter-spacing:.04em}.ai-recommend-button{display:inline-flex;width:auto;align-items:center;font-size:13px;font-weight:750;padding:6px 10px;box-shadow:0 2px 6px rgba(37,99,235,.20)}.ai-apply-button{font-weight:750}.ai-parameter-proposal{white-space:pre-wrap;background:#fff;border:1px solid #dbe5f1;border-radius:8px;min-height:44px}.ai-local-config{background:#f8fafc;border:1px solid #dbe5f1;border-radius:12px;padding:14px 16px;margin:14px 0}.ai-local-config h4{margin:0 0 10px;color:#284b78;font-weight:800}.ai-local-note{color:#667892;font-size:13px;line-height:1.55}.ai-local-actions{display:flex;gap:8px;flex-wrap:wrap}.ai-local-path{margin-top:10px;color:#718198;font-size:12px;overflow-wrap:anywhere}.ai-page-controls{display:flex;align-items:center;gap:12px;margin:12px 0}.ai-page-label{min-width:86px;text-align:center;font-weight:700;color:#38547a}.ai-markdown{line-height:1.75;background:#fff;border:1px solid #e3e8ef;border-radius:10px;padding:18px;margin-top:12px;overflow-wrap:anywhere}.ai-markdown h1{font-size:26px}.ai-markdown h2{font-size:22px}.ai-markdown h3{font-size:18px}.ai-markdown table{width:100%;border-collapse:collapse;margin:12px 0}.ai-markdown th,.ai-markdown td{border:1px solid #d7dee8;padding:8px;text-align:left}.ai-markdown th{background:#eef4ff}.ai-markdown pre{background:#f4f6f8;border-radius:6px;padding:12px;overflow:auto}.clean-workbench{margin-top:4px}.clean-heading{margin-bottom:12px}.clean-heading h3{margin:0 0 4px;font-weight:800;color:#13294b}.clean-subtitle{margin:0;color:#63758f;font-size:13px;line-height:1.5}.clean-overview{background:linear-gradient(135deg,#eaf3ff,#f8fbff);border:1px solid #c9dcf5;border-radius:12px;padding:12px 14px;margin:10px 0;color:#174a8b}.clean-overview-title{display:flex;gap:8px;align-items:center;font-weight:800;margin-bottom:5px}.clean-overview .shiny-text-output{font-size:12px}.clean-stage-tabs .nav:before,.clean-stage-tabs .nav:after{display:none!important;content:none!important}.clean-stage-tabs .nav{display:grid;grid-template-columns:1fr 1fr;gap:6px;margin:12px 0}.clean-stage-tabs .nav>li{margin:0;float:none!important;width:auto!important}.clean-stage-tabs .nav>li>a{display:flex;align-items:center;justify-content:center;min-height:42px;text-align:center;border-radius:8px;padding:8px 5px;font-size:12px;background:#edf2f8;color:#36577e}.clean-stage-tabs .nav>li.active>a{background:#2563eb;color:#fff}.clean-section{background:#fff;border:1px solid #dce5f2;border-radius:12px;padding:14px 14px 4px;margin:10px 0;box-shadow:0 2px 8px rgba(27,61,108,.05)}.clean-card-title{display:flex;align-items:center;gap:8px;color:#284b78;font-weight:800;margin-bottom:10px}.clean-tool-hint{color:#718198;font-size:12px;line-height:1.45;margin:8px 0 12px}.clean-subsection-title{font-weight:750;color:#36577e;margin:2px 0 5px}.clean-rule-list{background:#f4f7fb;border-radius:8px;padding:9px 10px;margin:10px 0;color:#49627f;font-size:12px;line-height:1.5;overflow-wrap:anywhere}.clean-divider{height:1px;background:#edf1f6;margin:14px 0}.clean-workbench .form-group{margin-bottom:12px}.clean-workbench .btn{border-radius:8px}.clean-action{width:100%;font-weight:650}.clean-button-row{display:flex;gap:8px}.clean-button-row .btn{flex:1}.clean-status{display:flex;align-items:flex-start;gap:8px;background:#eef6ff;border-left:4px solid #3b82f6;border-radius:8px;color:#174a8b;font-weight:600;margin:12px 0;padding:10px 12px;overflow-wrap:anywhere}.clean-help{color:#687a92;font-size:12px;line-height:1.5;margin:10px 2px}.clean-search-result{background:#f4f7fb;border-radius:8px;padding:8px;margin:8px 0;min-height:36px;overflow-wrap:anywhere}.clean-history-actions{margin-top:8px}.clean-export-card .shiny-download-link{display:block;text-align:center;border-radius:8px;margin-bottom:8px}.project-card{background:#fff;border:1px solid #cfe0f5;border-radius:12px;margin:14px 0;box-shadow:0 3px 12px rgba(37,99,235,.07)}.project-card summary{display:flex;align-items:center;gap:9px;padding:13px 14px;cursor:pointer;color:#1d4f91;font-weight:800;list-style:none}.project-card summary::-webkit-details-marker{display:none}.project-card summary:after{content:'+';margin-left:auto;font-size:20px;color:#6683aa}.project-card[open] summary:after{content:'−'}.project-card-body{padding:12px 14px 14px;border-top:1px solid #e7eff9}.project-card-hint{font-size:12px;color:#667892;line-height:1.5}.project-save-button{display:inline-block;margin-bottom:10px}.project-card-status{margin-top:10px;padding:8px 10px;background:#f4f7fb;color:#49627f;border-radius:8px;font-size:12px;overflow-wrap:anywhere}.ts-source-card{background:#fff;border:1px solid #cfe0f5;border-radius:12px;margin:16px 0;box-shadow:0 3px 12px rgba(37,99,235,.07)}.ts-source-card summary{display:flex;align-items:center;gap:9px;padding:14px 16px;cursor:pointer;color:#1d4f91;font-weight:750;list-style:none}.ts-source-card summary::-webkit-details-marker{display:none}.ts-source-card summary:after{content:'+';margin-left:auto;font-size:20px;color:#6683aa}.ts-source-card[open] summary:after{content:'−'}.ts-source-body{padding:4px 16px 16px;border-top:1px solid #e7eff9}.ts-source-hint{color:#667892;line-height:1.55;margin:10px 0}.ts-source-actions{display:flex;gap:8px;flex-wrap:wrap;margin-top:10px}.ts-source-actions .btn,.ts-source-actions .shiny-download-link{border-radius:8px}.ts-source-status,.ts-active-source{margin:10px 0;padding:9px 12px;border-radius:8px;overflow-wrap:anywhere}.ts-source-status{background:#f4f7fb;color:#49627f}.ts-active-source{background:#eaf3ff;color:#174a8b;font-weight:650}"))),
  tags$style(HTML(".container-fluid{max-width:1500px;padding-left:22px;padding-right:22px}.easyr-app-header{display:flex;align-items:center;gap:14px;padding:22px 2px 12px;padding-right:150px}.easyr-brand-mark{display:flex;align-items:center;justify-content:center;width:48px;height:48px;border-radius:14px;background:linear-gradient(135deg,#1d4f91,#2f6fec);color:#fff;font-size:22px;box-shadow:0 7px 18px rgba(37,99,235,.20)}.easyr-brand-title{font-size:27px;line-height:1.1;font-weight:900;color:#142b50}.easyr-brand-subtitle{margin-top:5px;color:#697b94;font-size:13px}.easyr-navigation>.tabbable>.nav-tabs{display:flex;align-items:center;gap:5px;flex-wrap:wrap;background:#fff;border:1px solid #dde6f2;border-radius:14px;padding:7px;margin:3px 0 15px;box-shadow:0 5px 16px rgba(31,78,139,.06)}.easyr-navigation>.tabbable>.nav-tabs:before,.easyr-navigation>.tabbable>.nav-tabs:after{display:none}.easyr-navigation>.tabbable>.nav-tabs>li{float:none;margin:0}.easyr-navigation>.tabbable>.nav-tabs>li>a{border:0!important;border-radius:9px;color:#36577e;font-weight:750;padding:10px 16px}.easyr-navigation>.tabbable>.nav-tabs>li>a:hover{background:#edf4ff;color:#174a8b}.easyr-navigation>.tabbable>.nav-tabs>li.active>a,.easyr-navigation>.tabbable>.nav-tabs>li.open>a{background:#2563eb!important;color:#fff!important}.easyr-navigation .dropdown-menu{border:1px solid #dce6f3;border-radius:11px;padding:6px;box-shadow:0 10px 26px rgba(31,78,139,.14)}.easyr-navigation .dropdown-menu>li>a{border-radius:7px;padding:9px 13px;color:#36577e;font-weight:650}.easyr-navigation .dropdown-menu>li.active>a{background:#eaf3ff;color:#174a8b}.workspace-page{background:#fff;border:1px solid #e1e8f1;border-radius:16px;padding:20px 22px;margin-bottom:24px;box-shadow:0 4px 14px rgba(31,78,139,.05)}.workspace-page-flat{padding-top:2px}.import-project-layout{margin-top:4px}.import-project-panel{background:#f8fafc;border:1px solid #e0e8f2;border-radius:14px;padding:18px;min-height:100%}@media(max-width:768px){.container-fluid{padding-left:12px;padding-right:12px}.easyr-app-header{padding-right:2px;padding-top:70px}.language-switch{right:12px;top:12px}.easyr-navigation>.tabbable>.nav-tabs>li>a{padding:8px 10px}.workspace-page{padding:15px}.import-project-panel{margin-bottom:12px}}")),
  tags$style(HTML(".startup-shell{min-height:calc(100vh - 30px);display:flex;align-items:center;justify-content:center;padding:60px 16px}.startup-card{width:min(500px,100%);background:#fff;border:1px solid #dce6f3;border-radius:18px;padding:32px;box-shadow:0 18px 45px rgba(31,78,139,.12)}.startup-brand{display:flex;align-items:center;gap:15px;margin-bottom:28px}.startup-brand-mark{display:flex;align-items:center;justify-content:center;width:52px;height:52px;border-radius:14px;background:#2563eb;color:#fff;font-size:22px}.startup-brand h1{font-size:30px;margin:0;color:#142b50;font-weight:900}.startup-brand p{margin:3px 0 0;color:#667892}.startup-actions .btn{width:100%;border-radius:9px;font-weight:750}.startup-primary{height:46px;font-size:16px}.startup-learning{height:46px;margin-top:10px;border:1px solid #9fc2f5;color:#174a8b;background:#eef5ff;font-size:16px}.startup-learning:hover{background:#deebfc;color:#123f79;border-color:#79a9e6}.startup-learning-hint{margin:7px 4px 0;color:#718198;font-size:12px;line-height:1.5}.startup-secondary{margin-top:3px}.startup-sample{margin-top:8px}.startup-divider{display:flex;align-items:center;gap:12px;color:#8a98aa;font-size:12px;margin:20px 0}.startup-divider:before,.startup-divider:after{content:'';height:1px;background:#e1e8f1;flex:1}.startup-status{min-height:20px;margin-top:15px;color:#667892;font-size:12px;line-height:1.5}.learning-shell{max-width:1320px;margin:0 auto;padding:18px 8px 30px}.learning-header{display:flex;align-items:center;gap:13px;margin:4px 0 14px;padding-right:150px}.learning-header-mark{display:flex;align-items:center;justify-content:center;width:46px;height:46px;border-radius:13px;background:linear-gradient(135deg,#174a8b,#2563eb);color:#fff;font-size:20px}.learning-header-copy{flex:1}.learning-header-title{font-size:25px;font-weight:900;color:#142b50}.learning-header-subtitle{color:#697b94;font-size:13px;margin-top:3px}.learning-back{margin-left:auto;white-space:nowrap;border-color:#b9cce5;color:#36577e;background:#fff}.learning-content{background:#fff;border:1px solid #e1e8f1;border-radius:16px;padding:18px 20px;box-shadow:0 4px 14px rgba(31,78,139,.05)}.easyr-workbench-layout{display:grid;grid-template-columns:310px minmax(0,1fr);gap:16px;align-items:start}.easyr-left-panel{position:sticky;top:10px;max-height:calc(100vh - 20px);overflow-y:auto;background:#fff;border:1px solid #dde6f2;border-radius:15px;padding:16px;box-shadow:0 5px 16px rgba(31,78,139,.07);scrollbar-width:thin}.easyr-left-panel h3{font-size:20px;margin:2px 0 14px}.easyr-left-panel .form-group{margin-bottom:10px}.easyr-left-panel label{font-size:13px;color:#36577e}.easyr-left-panel .form-control{height:38px}.easyr-left-panel .btn{border-radius:8px}.easyr-left-panel hr{margin:13px 0}.easyr-left-panel .project-card{margin:14px 0 0;box-shadow:none}.easyr-main-panel{min-width:0}.easyr-navigation>.tabbable>.nav-tabs{flex-wrap:nowrap;overflow-x:auto;overflow-y:hidden;scrollbar-width:thin}.easyr-navigation>.tabbable>.nav-tabs>li{flex:0 0 auto}.easyr-navigation>.tabbable>.nav-tabs>li>a{padding:9px 11px;font-size:13px;white-space:nowrap}.easyr-main-panel .dataset-status-bar{margin:0 0 10px}.easyr-main-panel .workspace-page{padding:16px 18px}.data-workspace-heading{display:flex;align-items:center;justify-content:space-between;margin-bottom:10px}.data-workspace-heading h2{font-size:24px;margin:0 0 4px;color:#173d70}.data-workspace-heading p{margin:0;color:#667892}.data-workspace-page>.tabbable>.nav-pills{display:inline-flex;gap:5px;background:#edf3fa;border-radius:10px;padding:5px;margin:8px 0 16px}.data-workspace-page>.tabbable>.nav-pills>li{float:none}.data-workspace-page>.tabbable>.nav-pills>li>a{border-radius:7px;padding:8px 16px;color:#36577e;font-weight:750}.data-workspace-page>.tabbable>.nav-pills>li.active>a{background:#2563eb;color:#fff}@media(max-width:1050px){.easyr-workbench-layout{grid-template-columns:270px minmax(0,1fr)}.easyr-left-panel{padding:13px}}@media(max-width:780px){.startup-shell{padding:75px 8px 25px}.startup-card{padding:24px}.learning-header{padding-right:0;flex-wrap:wrap}.learning-back{margin-left:59px}.learning-content{padding:14px}.easyr-workbench-layout{grid-template-columns:1fr}.easyr-left-panel{position:static;max-height:none}}")),
  tags$style(HTML(".startup-file-hint{margin:-8px 0 7px;color:#7a899d;font-size:12px}")),
  tags$style(HTML(".example-dataset-picker{margin-top:14px;padding-top:12px;border-top:1px solid #e5ebf3}.example-dataset-picker .form-group{margin-bottom:8px}.example-dataset-picker .btn{margin-bottom:7px}.example-dataset-description{min-height:34px;color:#6a7b91;font-size:12px;line-height:1.45}.startup-example-picker{margin-top:14px;text-align:left}.startup-example-picker .form-group{margin-bottom:8px}.startup-example-picker .startup-file-hint{margin:0 2px 8px;min-height:34px;line-height:1.45}.startup-example-picker .startup-sample{width:100%;margin-top:0;border-color:#b9cce5;color:#24558e;background:#f7faff}.startup-example-picker .startup-sample:hover{background:#eaf3ff;border-color:#91b6e6}")),
  tags$style(HTML(".ai-settings-hero{display:grid;grid-template-columns:minmax(0,1fr) 270px;gap:22px;align-items:center;margin-bottom:18px;padding:18px 20px;border:1px solid #d8e4f3;border-radius:16px;background:linear-gradient(135deg,#f8fbff,#eef5ff);overflow:hidden}.ai-settings-intro h3{margin:0 0 8px;color:#173d70}.ai-settings-intro p{margin:0;color:#60758f;line-height:1.65}.ai-provider-character-stage{min-height:190px;display:flex;align-items:center;justify-content:center}.ai-provider-character-stage>.shiny-panel-conditional{width:100%}.ai-character-card{position:relative;height:190px;border:0;border-radius:14px;overflow:hidden;background:transparent;box-shadow:none}.ai-character-image{width:100%;height:100%;display:block;object-fit:contain;object-position:center bottom}.ai-character-placeholder{height:100%;display:flex;align-items:center;justify-content:center;font-size:62px;color:#5a80b5;background:radial-gradient(circle at 50% 35%,#fff,#eaf3ff)}@media(max-width:900px){.ai-settings-hero{grid-template-columns:1fr;padding:15px}.ai-provider-character-stage{min-height:170px}.ai-character-card{height:170px}}")),
  tags$style(HTML(".workspace-page .btn{width:auto;padding:6px 10px;font-size:13px;line-height:1.35;border-radius:7px}.workspace-page .clean-action{width:auto}.workspace-page .clean-button-row{justify-content:flex-start;flex-wrap:wrap}.workspace-page .clean-button-row .btn{flex:0 0 auto}.workspace-page .clean-export-card .shiny-download-link{display:inline-block;width:auto;margin-right:6px}.workspace-page details.ai-parameter-panel,.workspace-page details.ts-source-card{width:auto;border:0;background:transparent;box-shadow:none;padding:0;margin:10px 0}.workspace-page details.ai-parameter-panel>summary,.workspace-page details.ts-source-card>summary{display:inline-flex;width:auto;align-items:center;gap:7px;margin:0;padding:6px 10px;border:1px solid #bfd3ec;border-radius:8px;background:#eef5ff;color:#174a8b;font-size:13px;font-weight:750;list-style:none;box-shadow:none}.workspace-page details.ai-parameter-panel>summary:hover,.workspace-page details.ts-source-card>summary:hover{background:#e1edfc;border-color:#91b6e6}.workspace-page details.ai-parameter-panel>summary::-webkit-details-marker,.workspace-page details.ts-source-card>summary::-webkit-details-marker{display:none}.workspace-page details.ai-parameter-panel>summary:after,.workspace-page details.ts-source-card>summary:after{content:'+';margin-left:3px;font-size:15px;color:#5d7fa8}.workspace-page details.ai-parameter-panel[open]>summary:after,.workspace-page details.ts-source-card[open]>summary:after{content:'−'}.workspace-page details.ai-parameter-panel[open],.workspace-page details.ts-source-card[open]{border:1px solid #d7e3f1;border-radius:10px;background:#fff;padding:10px 12px}.workspace-page details.ai-parameter-panel[open]>summary,.workspace-page details.ts-source-card[open]>summary{margin-bottom:8px}.workspace-page details.ai-parameter-panel .ai-parameter-badge{margin-left:2px;padding:1px 6px;font-size:10px}.workspace-page details.ts-source-card .ts-source-body{padding:10px 0 0;border-top:1px solid #e7eff9}")),
  tags$style(HTML(".easyr-navigation>.tabbable>.nav-tabs{display:grid;grid-template-columns:repeat(6,minmax(0,1fr));gap:6px;overflow:visible}.easyr-navigation>.tabbable>.nav-tabs>li{width:100%;min-width:0}.easyr-navigation>.tabbable>.nav-tabs>li>a{display:flex;align-items:center;justify-content:center;text-align:center;padding:10px 8px}.analysis-group-page{padding-top:10px}.workspace-subnav>.tabbable>.nav-pills{display:flex;align-items:center;gap:6px;flex-wrap:wrap;background:#edf3fa;border:1px solid #dce6f3;border-radius:11px;padding:5px;margin:0 0 16px}.workspace-subnav>.tabbable>.nav-pills:before,.workspace-subnav>.tabbable>.nav-pills:after{display:none}.workspace-subnav>.tabbable>.nav-pills>li{float:none;margin:0}.workspace-subnav>.tabbable>.nav-pills>li>a{border-radius:8px;padding:8px 14px;color:#36577e;font-size:13px;font-weight:750}.workspace-subnav>.tabbable>.nav-pills>li>a:hover{background:#fff;color:#174a8b}.workspace-subnav>.tabbable>.nav-pills>li.active>a{background:#2563eb;color:#fff;box-shadow:0 2px 7px rgba(37,99,235,.18)}@media(max-width:1050px){.easyr-navigation>.tabbable>.nav-tabs{grid-template-columns:repeat(3,minmax(0,1fr))}}@media(max-width:620px){.easyr-navigation>.tabbable>.nav-tabs{grid-template-columns:repeat(2,minmax(0,1fr))}.workspace-subnav>.tabbable>.nav-pills>li{flex:1 1 calc(50% - 6px)}.workspace-subnav>.tabbable>.nav-pills>li>a{display:block;text-align:center;padding:8px 6px}}")),
  tags$style(HTML(".function-plotter-shell{max-width:1400px;margin:0 auto;padding:18px 8px 30px}.function-plotter-content{background:#fff;border:1px solid #e1e8f1;border-radius:16px;padding:20px 22px;box-shadow:0 4px 14px rgba(31,78,139,.05)}.function-plotter-toolbar{display:flex;align-items:end;gap:14px;flex-wrap:wrap;padding:12px 14px 2px;background:#f4f7fb;border-radius:11px}.function-plotter-toolbar>.form-group{min-width:170px;margin-bottom:10px}.function-plotter-toolbar>.checkbox{margin:0 0 16px}.function-plotter-help{color:#667892;line-height:1.55;margin:12px 2px}.function-plotter-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;margin:12px 0}.function-plotter-card{border:1px solid #dbe5f1;border-radius:11px;padding:11px 12px 1px;background:#fbfdff}.function-plotter-card-title{font-weight:800;color:#174a8b;margin-bottom:7px}.function-plotter-style{margin:12px 0}.function-plotter-style>summary{display:inline-flex;align-items:center;padding:7px 11px;border:1px solid #bfd3ec;border-radius:8px;background:#eef5ff;color:#174a8b;font-weight:750;cursor:pointer;list-style:none}.function-plotter-style>summary::-webkit-details-marker{display:none}.function-plotter-style-body{border:1px solid #dbe5f1;border-radius:10px;padding:12px 13px 2px;margin-top:8px}.function-plotter-actions{display:flex;gap:8px;flex-wrap:wrap}.function-plotter-saved{color:#667892;font-size:12px;margin-top:7px;overflow-wrap:anywhere}@media(max-width:760px){.function-plotter-grid{grid-template-columns:1fr}.function-plotter-content{padding:14px}.function-plotter-toolbar>.form-group{min-width:140px}}")),
  tags$style(HTML(".function-plotter-content{padding:0;overflow:hidden}.function-plotter-workspace{display:grid;grid-template-columns:350px minmax(0,1fr);min-height:720px}.function-plotter-sidebar{background:#f8fafc;border-right:1px solid #dbe5f1;padding:16px 14px;overflow-y:auto}.function-plotter-sidebar-title{display:flex;align-items:center;gap:8px;font-size:19px;font-weight:850;color:#173d70;margin-bottom:4px}.function-plotter-sidebar-hint{font-size:12px;color:#6b7d93;line-height:1.5;margin:0 0 13px}.function-input-row{border-top:1px solid #dce5ef;padding:11px 0 8px}.function-type-line{display:grid;grid-template-columns:12px minmax(0,1fr) 30px;gap:7px;align-items:center}.function-type-line .form-group{margin:0}.function-type-line .form-control{height:31px;padding:3px 8px;font-size:12px}.function-expression-line{display:grid;grid-template-columns:auto minmax(0,1fr);gap:7px;align-items:center}.function-formula-line{margin:5px 30px 0 29px}.function-color-dot{width:10px;height:10px;border-radius:50%;display:block}.function-prefix{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#36577e;white-space:nowrap}.function-expression-input .form-group{margin:0}.function-expression-input .form-control{height:36px;border:0;border-bottom:2px solid #b9c9dc;border-radius:0;background:transparent;padding:5px 3px;box-shadow:none;font-size:15px}.function-expression-input .form-control:focus{border-color:#2563eb}.function-remove-button{border:0;background:transparent;color:#8a98aa;padding:5px!important}.function-remove-button:hover{color:#dc2626;background:#fee2e2}.function-domain-line{display:flex;align-items:center;gap:5px;margin:7px 0 0 29px;color:#63758f;font-size:12px}.function-domain-line .form-group{margin:0;width:82px}.function-domain-line .form-control{height:29px;padding:3px 6px;font-size:12px}.function-domain-symbol{white-space:nowrap}.function-add-button{width:100%;margin-top:7px;border:1px dashed #9bb7d9;background:#fff;color:#24558e}.function-plotter-sidebar hr{margin:15px 0}.function-plotter-checks{display:grid;grid-template-columns:1fr}.function-plotter-checks .checkbox{margin:5px 0}.function-plotter-style>summary{width:100%;justify-content:center}.function-plotter-style-body{padding:10px 10px 1px;background:#fff}.function-sidebar-actions{display:flex;gap:6px;margin-top:12px}.function-sidebar-actions>*{flex:1;text-align:center}.function-plotter-canvas{position:relative;padding:18px 22px 14px;min-width:0;background:#fff}.function-empty-hint{position:absolute;inset:0;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:8px;color:#8190a4;pointer-events:none}.function-empty-hint .fa{font-size:42px;color:#b6c8dd}.function-empty-hint strong{font-size:18px;color:#58708f}.function-empty-hint span{font-size:13px}@media(max-width:820px){.function-plotter-workspace{grid-template-columns:1fr}.function-plotter-sidebar{border-right:0;border-bottom:1px solid #dbe5f1}.function-plotter-canvas{padding:12px}.function-plotter-checks{grid-template-columns:repeat(3,1fr)}}")),
  language_switch_ui(),
  conditionalPanel("!output.workspace_ready && !output.learning_ready && !output.function_plotter_ready", startup_ui("startup")),
  conditionalPanel("output.learning_ready",
    tags$div(class = "learning-shell",
      tags$div(class = "learning-header",
        tags$div(class = "learning-header-mark", icon("graduation-cap")),
        tags$div(class = "learning-header-copy",
          tags$div(class = "learning-header-title", "RBench 学习模式"),
          tags$div(class = "learning-header-subtitle", "概率与统计 · 公式 · 模拟 · 互动演示")),
        actionButton("leave_learning", "返回开始界面", icon = icon("arrow-left"), class = "learning-back")),
      tags$div(class = "learning-content", teaching_mode_ui()))),
  conditionalPanel("output.function_plotter_ready",
    tags$div(class = "function-plotter-shell",
      tags$div(class = "learning-header",
        tags$div(class = "learning-header-mark", icon("chart-line")),
        tags$div(class = "learning-header-copy",
          tags$div(class = "learning-header-title", "RBench 函数绘图"),
          tags$div(class = "learning-header-subtitle", "输入函数 · 独立定义域 · 多曲线比较 · PNG 导出")),
        actionButton("leave_function_plotter", "返回开始界面", icon = icon("arrow-left"), class = "learning-back")),
      tags$div(class = "function-plotter-content", function_plotter_ui("function_plotter")))),
  conditionalPanel("output.workspace_ready",
    tags$div(class = "easyr-app-header",
      tags$div(class = "easyr-brand-mark", icon("chart-simple")),
      tags$div(class = "easyr-brand-title", "RBench 工作台")),
    tags$div(class = "easyr-workbench-layout",
    tags$aside(class = "easyr-left-panel",
      tags$div(class = "easyr-left-section", import_ui("import")),
      project_ui("project")),
    tags$main(class = "easyr-main-panel",
      dataset_bar_ui("import"),
      tags$div(class = "easyr-navigation",
        tabsetPanel(id = "main_navigation", selected = "data_workspace",
          tabPanel("数据工作区", value = "data_workspace",
            tags$div(class = "workspace-page data-workspace-page",
              tags$div(class = "data-workspace-heading",
                tags$div(tags$h2("数据工作区"), tags$p("左侧导入数据，在这里浏览内容并完成整理。"))),
              tabsetPanel(id = "data_workspace_tabs", selected = "preview", type = "pills",
                tabPanel("数据浏览", value = "preview", preview_ui("preview")),
                tabPanel("整理数据", value = "clean", workbench_ui("clean"))))),
          tabPanel("探索分析", value = "exploration_group",
            tags$div(class = "workspace-page workspace-page-flat analysis-group-page workspace-subnav",
              tabsetPanel(id = "exploration_navigation", selected = "analysis", type = "pills",
                tabPanel("统计与绘图", value = "analysis", analysis_ui("analysis")),
                tabPanel("主成分分析", value = "pca", pca_ui("pca")),
                tabPanel("K-means 聚类", value = "kmeans", kmeans_ui("kmeans"))))),
          tabPanel("回归分析", value = "regression_group",
            tags$div(class = "workspace-page workspace-page-flat analysis-group-page workspace-subnav",
              tabsetPanel(id = "regression_navigation", selected = "regression", type = "pills",
                tabPanel("线性回归", value = "regression", regression_ui("regression")),
                tabPanel("逻辑回归", value = "logistic_regression", logistic_regression_ui("logistic_regression")),
                tabPanel("高级回归", value = "advanced_regression", advanced_regression_ui("advanced_regression"))))),
          tabPanel("机器学习", value = "machine_learning_group",
            tags$div(class = "workspace-page workspace-page-flat analysis-group-page workspace-subnav",
              tabsetPanel(id = "machine_learning_navigation", selected = "decision_tree", type = "pills",
                tabPanel("决策树", value = "decision_tree", decision_tree_ui("decision_tree")),
                tabPanel("随机森林", value = "random_forest", random_forest_ui("random_forest")),
                tabPanel("支持向量机", value = "svm", svm_ui("svm")),
                tabPanel("模型比较", value = "model_comparison", model_comparison_ui("model_comparison"))))),
          tabPanel("时间序列", value = "timeseries", tags$div(class = "workspace-page workspace-page-flat", timeseries_ui("timeseries"))),
          tabPanel("AI 设置", value = "ai_settings", tags$div(class = "workspace-page", ai_settings_ui("ai_settings")))
        )
      )
    ))
  )
)
server <- function(input, output, session) {
  language_switch_server(input, output, session)
  ai_config <- ai_settings_server("ai_settings")
  directory <- reactive(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  workspace_started <- reactiveVal(FALSE)
  learning_started <- reactiveVal(FALSE)
  function_plotter_started <- reactiveVal(FALSE)
  output$workspace_ready <- reactive(workspace_started())
  output$learning_ready <- reactive(learning_started())
  output$function_plotter_ready <- reactive(function_plotter_started())
  outputOptions(output, "workspace_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "learning_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "function_plotter_ready", suspendWhenHidden = FALSE)
  project_channel <- new.env(parent = emptyenv())
  imported <- import_server("import")
  data <- workbench_server("clean", imported$data, directory, imported$name, project_channel)
  project_server("project", imported, project_channel, input, session)
  enter_workspace <- function() {
    learning_started(FALSE)
    function_plotter_started(FALSE)
    workspace_started(TRUE)
    session$onFlushed(function() {
      session$sendInputMessage("main_navigation", list(value = "data_workspace"))
      session$sendInputMessage("data_workspace_tabs", list(value = "preview"))
    }, once = TRUE)
  }
  enter_learning <- function() {
    workspace_started(FALSE)
    function_plotter_started(FALSE)
    learning_started(TRUE)
  }
  enter_function_plotter <- function() {
    workspace_started(FALSE)
    learning_started(FALSE)
    function_plotter_started(TRUE)
  }
  startup_server("startup", imported, project_channel, input, session, enter_workspace, enter_learning, enter_function_plotter)
  observeEvent(input$leave_learning, {
    learning_started(FALSE)
    workspace_started(FALSE)
  })
  observeEvent(input$leave_function_plotter, {
    function_plotter_started(FALSE)
    learning_started(FALSE)
    workspace_started(FALSE)
  })
  observeEvent(imported$revision(), {
    req(imported$revision() > 0L)
    enter_workspace()
  }, ignoreInit = TRUE)
  preview_server("preview", data, ai_config)
  analysis_server("analysis", data, directory)
  function_plotter_server("function_plotter", directory)
  teaching_mode_server(directory)
  regression_server("regression", data, directory, ai_config)
  logistic_regression_server("logistic_regression", data, directory, ai_config)
  advanced_regression_server("advanced_regression", data, directory, ai_config)
  random_forest_server("random_forest", data, directory, ai_config)
  decision_tree_server("decision_tree", data, directory, ai_config)
  model_comparison_server("model_comparison", data, directory, ai_config)
  svm_server("svm", data, directory, ai_config)
  pca_server("pca", data, directory, ai_config)
  kmeans_server("kmeans", data, directory, ai_config)
  timeseries_server("timeseries", data, directory, ai_config, imported$add)
}
shinyApp(ui, server)
