# Load data.table library
library(data.table)

# Load SNP array data (https://gdc.cancer.gov/about-data/publications/pancanatlas)
data = fread("pan-cancer-analysis/data/broad.mit.edu_PANCAN_Genome_Wide_SNP_6_whitelisted.seg", sep="\t")

# Load annotation data (https://gdc.cancer.gov/about-data/publications/pancanatlas)
ann = fread("pan-cancer-analysis/data/merged_sample_quality_annotations.tsv", sep="\t")

# Match annotation data
ann_matched = ann[which(ann$aliquot_barcode %in% data$Sample),]

# Which unique cancer types are available?
types = unique(ann_matched$`cancer type`)

# Function to analyse TCGA SNP array data for copy number alterations in specified region
cn_analyse = function(name, chr, pos1, pos2, dir) {
  
  # Initialize overviews
  overview = list()
  summary = NULL
  complete = NULL
  
  # Min and max for given chromosome
  pos_min = min(as.numeric(data$Start[which(data$Chromosome==chr)]))
  pos_max = max(as.numeric(data$Start[which(data$Chromosome==chr)]))
  
  # Iterate through cancer types
  for (type in types) {
    
    # Select samples from given cancer type AND derived from tumour (https://gdc.cancer.gov/resources-tcga-users/tcga-code-tables/sample-type-codes) AND not marked 'do not use'
    samples = ann_matched$aliquot_barcode[which(ann_matched$`cancer type` == type & substr(ann_matched$aliquot_barcode,14,15)%in%paste0("0",1:9) & ann_matched$Do_not_use == FALSE)]
    
    # Initialize results table
    type_overview = NULL
    
    # Iterate through samples
    for (sample_i in 1:length(samples)) {
      
      # Select SNP array data matching with chr and pos1 and pos2
      sample = samples[sample_i]
      sample_data = data[which(data$Sample==sample & data$Chromosome==chr & data$Start<=pos1& data$End>=pos1),]
      
      # In case no segmentation, take mean as CN value
      cn_val = mean(sample_data$Segment_Mean)
      
      # Check if CN value is valid
      if (!is.na(cn_val)) {
        
        # If CN value < -0.3, call as loss
        if (cn_val < (-0.3)) {
          cn_val = "loss"
        }
        # If CN value > 0.3, call as gain
        else if (cn_val > 0.3) {
          cn_val = "gain"
        }
        # If -0.3 <= CN value <= 0.3, call as normal
        else {
          cn_val = "normal"
        }
        
      }
      # Or mark as NA
      else {
        cn_val = NA
      }
      
      # Save results to type_overview
      type_overview = rbind(type_overview, c(sample, cn_val))
      
      # Print progression
      if((sample_i-1)%%25 == 0) {
        print(paste0("Progression: analyzing ", type, " sample ",sample_i,"/",length(samples)))
      }
    }
    
    # Save results
    colnames(type_overview) = c("sample","CNA")
    overview[[type]] = type_overview
    complete = rbind(complete, type_overview)
    
    total = length(which(!is.na(type_overview[,"CNA"])))
    gain = length(which(type_overview[,"CNA"]=="gain"))
    loss = length(which(type_overview[,"CNA"]=="loss"))
    normal = length(which(type_overview[,"CNA"]=="normal"))
    
    summary = rbind(summary,c(type,
                              total,
                              gain/total,
                              loss/total,
                              normal/total
    ))
  }
  
  # Save RDS files
  saveRDS(overview, paste0(dir,"/",name,"-overview.RDS"))
  saveRDS(summary, paste0(dir,"/",name,"-summary.RDS"))
  saveRDS(complete, paste0(dir,"/",name,"-complete.RDS"))
  
}

# Function to read stored cn_analyse data
cn_read = function(name, dir) {
  
  # Read RDS files
  cn_overview <<- readRDS(paste0(dir,"/",name,"-overview.RDS"))
  cn_summary <<- readRDS(paste0(dir,"/",name,"-summary.RDS"))
  cn_complete <<- readRDS(paste0(dir,"/",name,"-complete.RDS"))
  
}

# For all analyses: use GRCh38 locations for genes (https://www.genecards.org/)
# Analysis of IGH gene
cn_analyse(name = "IGH",
           chr = 14,
           pos1 = 105586437,
           pos2 = 106879844,
           dir = "pan-cancer-analysis/res")

# Read and process TCGA cohorts annotation
cohorts = read.csv("pan-cancer-analysis/data/tcga-cohorts.txt", sep="\t", stringsAsFactors = F, check.names = F)
capitalize = function(x) {
  y = strsplit(x, " ")[[1]]
  paste(toupper(substring(y, 1,1)), substring(y, 2),
        sep="", collapse=" ")
}
rownames(cohorts) = cohorts$Cohort

# Function to create frequency plot
create_cn_freq_plot = function(name, main) {
  
  # Set colors
  color_1 = rgb(.25,.25,.25)
  color_2 = rgb(.75,.75,.75)
  graphics.off()
  
  # Read relevant data
  cn_read(name = name,
          dir = "pan-cancer-analysis/res")
  
  # Initiate PNG
  img_file= paste0("pan-cancer-analysis/res/",name,"-CNV.png")
  png(img_file, res=600, width=5000, height=4500)  
  {
    # Set margin
    par(mar=c(5,5,5,5))
    
    # Set limits of axes
    xlim = c(0,10)
    ylim = c(0,33)
    
    # Initiate plot
    plot(type="n",
         x = c(-12,10),
         y = c(0,33),
         axes = F,
         xlab = "",
         ylab = "",
         main = "",
         bty = "l", 
         xaxs = "i", 
         yaxs = "i")
    
    # Draw axes
    xat = seq(from = 0, to = 10, by = 2.5)
    yat = seq(from = 0, to = 33, by = 1)
    segments(xat, 0, xat, 33, col  ="#eeeeee", lwd=1.4, xpd=T)
    segments(xat, 0, xat, -0.5, col  ="#b1b1b1", lwd=1.4, xpd=T)
    text(xat, -0.5, pos = 1, col = "#333333", labels = c("0%","25%","50%","75%","100%"), xpd = T)  
    text(5, -2.5, pos = 1, col = "#333333", labels = "Fraction affected tumours", xpd = T)  
    text(5, 35.5, pos = 1, col = "#333333", labels = main, xpd = T, font=2)  
    
    # Exclude DLBC and LAML from visualisation and order matrix
    freq_matrix = as.matrix(cn_summary[which(cn_summary[,1] != "DLBC" & cn_summary[,1] != "LAML"),])
    freq_matrix = freq_matrix[order(as.numeric(freq_matrix[,5]),decreasing = T),]
    rownames(freq_matrix) = freq_matrix[,1]
    
    # Plot bars per tumour type
    for (i in 1:nrow(freq_matrix)) {
      p1 = as.numeric(freq_matrix[i,3])*10
      p2 = as.numeric(freq_matrix[i,4])*10
      y=i-0.5+2
      rect(0,y-0.4,p1,y+0.4,border="white",col=color_1, lwd=1.4)
      rect(p1,y-0.4,p1+p2,y+0.4,border="white",col=color_2, lwd=1.4)
      l=capitalize(cohorts[rownames(freq_matrix)[i], "Disease Name"])
      l=stringr::str_replace(string = l, pattern = "And ", replacement = "and ")
      text(-0.25,y,adj=1, col="#333333", labels=paste0(l, " (n=", as.numeric(freq_matrix[rownames(freq_matrix)[i], 2]),")"), xpd=T)  
    }
    
    # Plot pan-cancer mean bars
    p1 = mean(as.numeric(freq_matrix[,3]))*10
    p2 = mean(as.numeric(freq_matrix[,4]))*10
    y=1
    rect(0,y-0.4,p1,y+0.4,border="white",col=color_1, lwd=1.4)
    rect(p1,y-0.4,p1+p2,y+0.4,border="white",col=color_2, lwd=1.4)
    text(-0.25,y,adj=1, col="#333333", labels="Pan-cancer (mean)", xpd=T)  
    
    # Print pan-cancer means
    print(p1)
    print(p2)
    print(p1+p2)
    segments(0,0,0,33,xpd=T,col="#B1B1B1",lwd=1.4)
    segments(0,0,10,0,col="#B1B1B1",lwd=1.4,xpd=T)
    y = 1
    rect(5.25,y-.4,5.75,y+.4,col=color_1,border="white",lwd=1.4)
    text(5.75,y,col="#333333", labels = "gain", pos=4)
    
    y = 1
    rect(7.75,y-.4,8.25,y+.4,col=color_2,border="white",lwd=1.4)
    text(8.25,y,col="#333333", labels = "loss", pos=4)
  }
  
  # Finalise and open PNG
  dev.off()
  system(paste0("open ",img_file))
}

# Read IGH data
create_cn_freq_plot("IGH", "") #or use main=substitute(paste(bold("Copy number alterations in "), bolditalic('IGH'))))
