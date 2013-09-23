#数据预处理
#判断包是否存在，不存在则安装
load.lib <- function(package_name) {
    pkg <- length( grep(package_name,library()) ) >0;
    if ( !pkg ) {
        print(paste("Installing package:",package_name,sep=""));
        install.packages(package_name);
    }
    print(paste("loading package:",package_name,sep=""));
    library(package_name,character.only = TRUE);
}
 load.lib("tm")
