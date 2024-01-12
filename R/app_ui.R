#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(title="",
                 windowTitle="GWASplot"
                 )
      )
  navbarPage(
    title = "GWASPlot",#网页logo
    inverse = T,
    theme = shinytheme("cerulean"),
    header = NULL,
    tabPanel("基因型分析",
             h4(strong("上传基因型文件")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadhmp_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                             ),
                 actionButton("view_uploadhmp_exm",
                              "查看示例文件"),
                 fileInput("uploadhmp",
                           "如果选择了否，请上传Hapmap格式文件",
                           accept = c(".txt", ".hmp"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL
                           )
                 ),
               mainPanel(
                 helpText("注意事项："),
                 helpText("1）基因型文件标题行必须以rs alleles chrom pos strand assembly center protLSID assayLSID panel QCcode开头"),
                 helpText("2）具体格式参照示例文件"),
                 dataTableOutput("hmp_exm")
                 )
               ),
             h4(strong("基因型系统发育树")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Tree_type",
                             "选择系统发育树的形状",
                             choices = c("rectangular", "dendrogram","slanted","ellipse","roundrect","fan","circular","inward_circular","radial","equal_angle","daylight","ape")),
                 numericInput("tree_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("tree_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("tree_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("structural_analyse",
                              '开始',
                              icon = icon('play')
                              ),
                 downloadButton("download_tree", "下载")
                 ),
               mainPanel(plotOutput("PhyloTree_plot")
                         )
               ),
             h4(strong("基因型PCA分析-2D")
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 numericInput("X_2D",
                              "选择X轴",
                              value = '1',
                              min = '1'),
                 numericInput("y_2D",
                              "选择Y轴",
                              value = '2',
                              min = '1'),
                 numericInput("PCA_plot_2D_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("PCA_plot_2D_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("PCA_plot_2D_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("PCA_2D",
                              '开始',
                              icon = icon('play')),
                 downloadButton("download_PCA_plot_2D", "下载")
               ),
               mainPanel(plotOutput("PCA_plot_2D"))
             ),
             h4(strong("基因型PCA分析-3D")
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 numericInput("X_3D",
                              "选择X轴",
                              value = '1',
                              min = '1'),
                 numericInput("y_3D",
                              "选择Y轴",
                              value = '2',
                              min = '1'),
                 numericInput("Z_3D",
                              "选择Z轴",
                              value = '3',
                              min = '1'),
                 numericInput("PCA_plot_3D_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("PCA_plot_3D_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("PCA_plot_3D_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("PCA_3D",
                              '开始',
                              icon = icon('play')
                 ),
                 downloadButton("download_PCA_plot_3D",
                                "下载")
               ),
               mainPanel(plotOutput("PCA_plot_3D")
               )
             )
    ),
    tabPanel("表型分析",
             h4(strong("上传表型文件")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadpheno_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_uploadpheno_exm",
                              "查看示例文件"),
                 fileInput("uploadpheno",
                           "如果选择了否，上传表型文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE)
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）表型文件标题行必须以Line Rep Year Loc开头"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("pheno_exm")
               )
             ),
             h4(strong("计算BLUP值")
             ),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("blup_table_format",
                              "选择下载表格格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 actionButton("Calblup",
                              "计算",
                              icon = icon('edit')
                 ),
                 downloadButton("download_blup",
                                "下载")
               ),
               mainPanel(dataTableOutput("Bluptable")
               )
             ),
             h4(strong("平均值分布直方图")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("Histplot_width",
                              "下载图片宽度",
                              value = 10),
                 numericInput("Histplot_height",
                              "下载图片高度",
                              value = 10),
                 radioButtons("Histplot_faormat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 downloadButton("download_histplot", "下载")
               ),
               mainPanel(plotOutput("Histplot_mean")
               )
             )
    ),
    tabPanel("GWAS分析",
             h4(strong("上传基因型文件")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadgeno_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")),
                 actionButton("view_uploadgeno_exm",
                              "查看示例文件"),
                 fileInput("uploadgeno",
                           "如果选择了否，上传处理后的Hapmap格式文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE)
               ),
               mainPanel(dataTableOutput("geno_exm")
               )
             ),
             h4(strong("上传表型文件")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadpheno_GWAS_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")),
                 actionButton("view_uploadpheno_GWAS_exm",
                              "查看示例文件"),
                 fileInput("uploadpheno_GWAS",
                           "如果选择了否，上传处理后的表型数据",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE)
               ),
               mainPanel(dataTableOutput("pheno_GWAS_exm")
               )
             ),
             h4(strong("运行GWAS")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("GWAS_model",
                             "选择GWAS模型",
                             choices = c("BLINK", "CMLM", "GLM", "MLM", "MMLM", "SUPER", "FarmCPU", "EMMAxP3D")
                 ),
                 numericInput("PCA_num",
                              "PCA数",
                              value = '3'),
                 radioButtons("GWAS_table_format",
                              "选择下载表格格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 actionButton("RunGWAS",
                              "开始",
                              icon = icon('random')),
                 downloadButton("download_GWAS",
                                "下载")
               ),
               mainPanel(dataTableOutput("GWASresult")
               )
             ),
             h4(strong("QQ图")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("QQ_width",
                              "下载图片宽度",
                              value = 10),
                 numericInput("QQ_height",
                              "下载图片高度",
                              value = 10),
                 radioButtons("QQ_farmat",
                              "下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Plot_QQ",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_QQ",
                                "下载")
               ),
               mainPanel(plotOutput("QQ_plot")
               )
             ),
             h4(strong("曼哈顿图")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("Manhattan_y",
                              "选择阈值-log10（P）",
                              value = '3'),
                 numericInput("Manhattan_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("Manhattan_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("Manhattan_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Plot_Manhattan", "开始", icon = icon('play')
                 ),
                 downloadButton("download_Manhattan", "下载")
               ),
               mainPanel(plotlyOutput("Manhattan_plot", width = "100%", height = "400px")
               )
             ),
             h4(strong("提取SNP对应的基因")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadGXF_exm","选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 fileInput("uploadGXF",
                           "如果选择了否，上传GXF文件(gff、gtf、gff3等)",
                           accept = c(".txt", ".gff", ".gtf", ".gff", ".gff3", ".GFF", ".GTF", ".GFF", ".GFF3"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 numericInput("up_down_stream",
                              "选择上下游范围（bp）",
                              value = '100000'),
                 numericInput("Significant_SNP",
                              "GWAS结果设置p值筛选SNP",
                              value = '0.01'),
                 radioButtons("SNP_Gene_format",
                              "选择下载表格格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 actionButton("Extract",
                              "提取",
                              icon = icon('play')
                 ),
                 downloadButton("download_SNP_Gene",
                                "下载")
               ),
               mainPanel(dataTableOutput("SNP_Gene")
               )
             ),
             h4(strong("LD热图")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadLDhmp_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_uploadLDhmp_exm",
                              "查看示例文件"),
                 fileInput("uploadLDhmp",
                           "如果选择了否，请上传Hapmap格式文件",
                           accept = c(".txt", ".hmp"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL)
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）确保基因型格式为0/0或A/A形式"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("LDhmp_exm")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 textInput("LD_Chromosome",
                           "选择染色体",
                           value = "1"),
                 numericInput("LD_left",
                              "选择起始位置",
                              value = '1000000'),
                 numericInput("LD_right",
                              "选择终止位置",
                              value = '5000000'),
                 numericInput("LD_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("LD_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("LD_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Plot_LD",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_LD",
                                "下载")
               ),
               mainPanel(
                 plotOutput("LD_plot")
               )
             )
    ),
    tabPanel("表达量分析",
             h4(strong("相关性分析")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Uploadtraits_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_Uploadtraits_exm",
                              "查看示例文件"),
                 fileInput("Uploadtraits",
                           "如果选择了否，请上传文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 selectInput("Cor_method",
                             "选择分析方法",
                             choices = c("pearson", "kendall", "spearman")
                 ),
                 radioButtons("Cor_format",
                              "选择下载表格格式",
                              choices = c("csv", "txt")
                 ),
                 numericInput("Cor_heatmap_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("Cor_heatmap_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("Cor_heatmap_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Cal_corr",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_Cor",
                                "下载表格"),
                 downloadButton("download_Cor_heatmap",
                                "下载图片")
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）表达量文件标题行必须以ID开头"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("traits_exm"),
                         dataTableOutput("Cor_tab"),
                         plotOutput("Cor_heatmap")
               )
             ),
             h4(strong("聚类分析")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("Hcluster_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("Hcluster_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("Hcluster_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Hcluster_plot",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_Hcluster",
                                "下载图片")
               ),
               mainPanel(plotOutput("Hcluster")
               )
             ),
             h4(strong("火山图")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("UploadVolcano_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_UploadVolcano_exm",
                              "查看示例文件"),
                 fileInput("UploadVolcano",
                           "如果选择了否，上传差异分析文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 numericInput("Volcano_x_left",
                              "选择x轴范围（原点左侧）",
                              value = -10),
                 numericInput("Volcano_x_right",
                              "选择x轴范围（原点右侧）",
                              value = 10),
                 numericInput("Volcano_p",
                              "选择p值",
                              value = 0.05),
                 numericInput("Volcano_FC",
                              "选择变异倍数",
                              value = 2),
                 numericInput("Volcano_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("Volcano_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("Volcano_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Volcano_plot",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_Volcano",
                                "下载图片")
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）文件标题行必须以GeneID log2FoldChange pvalue开头"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("Volcano_exm"),
                         plotOutput("Volcano")
               )
             ),
             h4(strong("PCA分析")
             ),
             sidebarLayout(
               sidebarPanel(
                 h4("1、上传表达量数据"),
                 selectInput("Upload_pcadata_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_Upload_pcadata_exm",
                              "查看示例文件"),
                 fileInput("Upload_pcadata",
                           "如果选择了否，请上传文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 h4("2、上传样本信息表"),
                 selectInput("Upload_pcadata_info_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_Uploadpcadata_info_exm",
                              "查看示例文件"),
                 fileInput("Upload_pcadata_info",
                           "如果选择了否，请上传文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 h4("3、选择绘图类型"),
                 selectInput("pca_type",
                             "",
                             choices = c("screeplot","biplot","pairsplot","loadingsplot")),
                 actionButton("pca_plot",
                              "开始",
                              icon = icon('play')
                 ),
                 radioButtons("pca_tab_format", "选择下载表格格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 downloadButton("download_pca_tab",
                                "下载表格"),
                 numericInput("pca_pic_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("pca_pic_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("pca_pic_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 downloadButton("download_pca_pic",
                                "下载图片")
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）样本信息表不需要表头，第一列为样本名称，第二列为分组信息"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("pcadata_exm"),
                         dataTableOutput("pcadata_info_exm"),
                         dataTableOutput("pca_tab"),
                         plotOutput("pca_plot")
               )
             ),
             h4(strong("热图")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Uploadheatmap_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_Uploadheatmap_exm",
                              "查看示例文件"),
                 fileInput("Uploadheatmap",
                           "如果选择了否，上传基因表达量文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE),
                 radioButtons("heatmap_scale",
                              "选择标准化对象",
                              choices = c("row", "column", "none")
                 ),
                 numericInput("heatmap_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("heatmap_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("heatmap_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("heatmap_plot",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_heatmap",
                                "下载图片")
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）文件标题行必须以ID开头"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("heatmap_exm"),
                         plotOutput("heatmap")
               )
             )
    ),
    tabPanel("功能富集分析",
             h4(strong("上传注释文件")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("UploadGeneInfo_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_UploadGeneInfo_exm","查看示例文件"),
                 fileInput("GeneInfo",
                           "如果选择了否，请上传注释文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE)
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）注释文件标题必须有GID GO KEGG Pathway"),
                         helpText("2）建议使用EggNOG注释工具得到的结果，要是要注意去掉表头的#"),
                         helpText("3）具体格式参照示例文件"),
                         dataTableOutput("GeneInfo_exm")
               )
             ),
             h4(strong("上传基因集")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("UploadGeneList_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_UploadGeneList_exm",
                              "查看示例文件"),
                 fileInput("GeneList",
                           "如果选择了否，上传基因集文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = TRUE)
               ),
               mainPanel(helpText("注意事项："),
                         helpText("1）基因集文件标题必须以GID开头"),
                         helpText("2）具体格式参照示例文件"),
                         dataTableOutput("GeneList_exm")
               )
             ),
             h4(strong("GO富集分析")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("GO_pvalue",
                              "选择p值",
                              value = '1'),
                 radioButtons("GO_table_format",
                              "选择下载格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 actionButton("Run_GO",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_GO_table",
                                "下载")
               ),
               mainPanel(dataTableOutput("GO_result")
               )
             ),
             h4(strong("KEGG富集分析")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("KEGG_pvalue",
                              "选择p值",
                              value = '1'),
                 radioButtons("KEGG_table_format",
                              "选择下载表格格式",
                              choices = c("csv","tsv", "txt")
                 ),
                 actionButton("Run_KEGG",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_KEGG_table",
                                "下载")
               ),
               mainPanel(dataTableOutput("KEGG_result")
               )
             ),
             h4(strong("GO富集可视化")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("BP_num",
                              "展示BP数量",
                              value = '10'),
                 numericInput("CC_num",
                              "展示CC数量",
                              value = '10'),
                 numericInput("MF_num",
                              "展示MF数量",
                              value = '10'),
                 numericInput("GO_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("GO_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("GO_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Plot_GO",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_GO",
                                "下载")
               ),
               mainPanel(plotlyOutput("GO_plot",
                                      width = "100%",
                                      height = "400px")
               )
             ),
             h4(strong("KEGG富集可视化")
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("KEGG_num",
                              "展示通路数量",
                              value = '10'),
                 numericInput("KEGG_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("KEGG_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("KEGG_farmat",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("Plot_KEGG",
                              "开始",
                              icon = icon('play')
                 ),
                 downloadButton("download_KEGG",
                                "下载")
               ),
               mainPanel(plotlyOutput("KEGG_plot",
                                      width = "100%",
                                      height = "400px")
               )
             )
    ),
    tabPanel("PPI可视化",
             h4(strong("基于STRING分析结果可视化")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("uploadppi_exm",
                             "选择是否使用示例文件",
                             choices = c("TRUE","FALSE")
                 ),
                 actionButton("view_uploadppi_exm",
                              "查看示例文件"),
                 fileInput("uploadppi",
                           "如果选择了否，请上传网络文件",
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected",
                           capture = NULL
                 ),
                 radioGroupButtons(
                   inputId = "followdeg",
                   label = "节点大小跟随degree变化",
                   choices = c("Yes", "No"),
                   justified = TRUE
                 ),
                 conditionalPanel(
                   condition = "input.followdeg == 'Yes'",
                   radioGroupButtons(
                     inputId = "followdeg_name",
                     label = "节点名称是否显示",
                     choices = c("Yes", "No"),
                     justified = TRUE
                   ),
                   conditionalPanel(
                     condition = "input.followdeg_name == 'Yes'",
                     radioGroupButtons(
                       inputId = "followdeg_name_size",
                       label = "节点名称字体大小是否跟随degree变化",
                       choices = c("Yes", "No"),
                       justified = TRUE
                     ),
                     conditionalPanel(
                       condition = "input.followdeg_name_size == 'Yes'",
                       sliderInput("followdeg_name_size_num",
                                   "相对字体大小",
                                   value = 4,
                                   min = 1,
                                   max = 9)
                     ),
                     conditionalPanel(
                       condition = "input.followdeg_name_size == 'No'",
                       sliderInput("followdeg_name_size_num_custom",
                                   "自定义字体大小",
                                   value = 4,
                                   min = 1,
                                   max = 9)
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.followdeg == 'No'",
                   radioGroupButtons(
                     inputId = "nofollowdeg_name",
                     label = "节点名称是否显示",
                     choices = c("Yes", "No"),
                     justified = TRUE
                   ),
                   conditionalPanel(
                     condition = "input.nofollowdeg_name == 'Yes'",
                     sliderInput("nofollowdeg_name_size",
                                 "字体大小",
                                 value = 4,
                                 min = 1,
                                 max = 9)
                   )
                 ),
                 selectInput("ppi_theme",
                             "选择主题",
                             choices = c("layout.auto", "layout.circle",	"layout.davidson.harel",	"layout.gem",	"layout.graphopt",	"layout.grid",	"layout.grid.3d",	"layout.kamada.kawai",	"layout.lgl",	"layout.mds",	"layout.random",	"layout.reingold.tilford",	"layout.sphere",	"layout.spring",	"layout.star",	"layout.sugiyama",	"layout.svd",	"layout_as_star",	"layout_as_tree",	"layout_components",	"layout_in_circle",	"layout_nicely",	"layout_on_grid",	"layout_on_sphere",	"layout_randomly",	"layout_with_dh",	"layout_with_drl",	"layout_with_fr",	"layout_with_gem",	"layout_with_graphopt",	"layout_with_kk",	"layout_with_lgl",	"layout_with_mds",	"layout_with_sugiyama"),
                             selected = "layout.auto"),
                 colorPickr(
                   inputId = "ppi_color",
                   label = "选择主题颜色",
                   width = "100%",
                   "#B2DF8A"
                 ),
                 numericInput("ppi_width",
                              "选择下载图片宽度",
                              value = 10),
                 numericInput("ppi_height",
                              "选择下载图片高度",
                              value = 10),
                 radioButtons("ppi_format",
                              "选择下载图片格式",
                              choices = c("jpeg", "pdf", "png", "svg", "tiff")
                 ),
                 actionButton("plot_ppi",
                              '开始',
                              icon = icon('play')
                 ),
                 downloadButton("download_ppi",
                                "下载")
               ),
               mainPanel(
                 helpText("注意事项："),
                 helpText("1）支持STRING数据库结果可视化"),
                 helpText("2）标题行必须以node1 node2 combined_score开头"),
                 helpText("3）具体格式参照示例文件"),
                 dataTableOutput("ppi_exm"),
                 plotOutput("ppi_plot")
               )
             )
    ),
    tabPanel("About",
             fluidRow(
               column(6,
                      panel_div(
                        class_type = "info",
                        panel_title = "关于该软件",
                        content = "这是一个GWAS分析及可视化的软件，后续我们会陆续加入其他功能，欢迎使用！")
               ),
               column(6,
                      panel_div(
                        class_type = "info",
                        panel_title = "安装软件",
                        content = HTML("本软件使用R/Shiny开发完成，可以在R环境中直接使用：",
                                       "<br>",
                                       "1）如果您习惯于使用R环境，请下载本软件对应的R包<a href='https://github.com/zhaojhweb/' target='_top'>点击此处</a>",
                                       "<br>",
                                       "2）如果您想免于R的安装过程，请直接下载本软件（仅限windows用户）<a href='https://github.com/zhaojhweb/' target='_top'>点击此处</a>")
                      )
               )
             ),
             br(),
             fluidRow(
               column(6,
                      panel_div(
                        class_type = "info",
                        panel_title = "使用说明",
                        content = HTML("使用过程中输入文件需要严格按照示例文件的格式")
                      )
               ),
               column(6,
                      panel_div(
                        class_type = "info",
                        panel_title = "联系作者",
                        content = HTML("如果使用过程中遇到问题或者有更好的建议，欢迎联系！",
                                       "<br>",
                                       "邮箱：","<a href='mailto:zhaojh96@outlook.com?Subject=Shiny%20Help' target='_top'>点击此处</a>")
                      )
               )
             )
    )
  )
}
