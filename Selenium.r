library(RSelenium)
library(ggplot2)
library(plotly)
library(ggthemes)

setwd("C:/Users/Guillaume/Documents/R/win-library/3.2/RSelenium/bin")
shell("java -jar selenium-server-standalone.jar")

setwd("C:/Users/Guillaume/Documents/Ravioli")
RSelenium::startServer()
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()

#Create empty Ravioli database#
df.Comments = data.frame(Text = character(0), Children = numeric(0), Score = numeric(0), Date = numeric(0), Week = numeric(0))
Week = 0
remDr$navigate("https://www.reddit.com/r/montreal/search?q=Freedom+Friday+author%3AAutoModerator&restrict_sr=on&sort=new&t=all")


for(i in 1:3){#look in the first 3 pages of #
page_tree.Current=htmlParse(remDr$getPageSource()[[1]])
Pages = xpathSApply(page_tree.Current, "//header[@class='search-result-header']/a", xmlGetAttr, "href")
NextSearch.Page = xpathSApply(page_tree.Current, "//a[@rel='nofollow next']", xmlGetAttr, "href")

for(i in 1:length(Pages)){

remDr$navigate(Pages[i])
Sys.sleep(10) 
 
page_tree.Current=htmlParse(remDr$getPageSource()[[1]])


ParentComments = xpathSApply(page_tree.Current,"//div[@class='sitetable nestedlisting']/div[@data-type='comment' and @data-author='kornikopic']/div/form",xmlValue)

Ravioli.Comment.Number = grep("ravioli", ParentComments) #search for the ravioli comment

if(length(Ravioli.Comment.Number) == 0){ Ravioli.Comment.Number = 1} #if no ravioli comment, just use the first comment

Ravioli.Comment.Text = xpathSApply(page_tree.Current,paste0("//div[@class='sitetable nestedlisting']/div[@data-type='comment' and @data-author='kornikopic'][", Ravioli.Comment.Number, "]/div/form"),xmlValue)

Ravioli.Comment.Score = xpathSApply(page_tree.Current,paste0("//div[@class='sitetable nestedlisting']/div[@data-type='comment' and @data-author='kornikopic'][", Ravioli.Comment.Number, "]/div/p/span[@class='score unvoted']"),xmlGetAttr, "title")

Ravioli.Comment.Children = gsub("[^0-9]", "", xpathSApply(page_tree.Current,paste0("//div[@class='sitetable nestedlisting']/div[@data-type='comment' and @data-author='kornikopic'][", Ravioli.Comment.Number, "]/div/p/a[@class='numchildren']"),xmlValue))

DateTime = as.POSIXct(as.numeric(xpathSApply(page_tree.Current,"//div[@id='siteTable']/div[1]",xmlGetAttr, "data-timestamp"))/1000, origin="1970-01-01", tz = "UTC")

Week = Week + 1
if(length(ParentComments>0)){
	df.Comments = rbind(
			df.Comments,
			data.frame(Text = Ravioli.Comment.Text, Children = as.numeric(Ravioli.Comment.Children), Score = as.numeric(Ravioli.Comment.Score), Date = DateTime, Week = Week)
		)
}
}

#go to next search page
remDr$navigate(NextSearch.Page)
Sys.sleep(10) 
}

save(df.Comments, file = "dfcomments.RData")

#df.Comments = df.Comments[-42,]
df.Comments$Text = gsub("\\n", "<br>", df.Comments$Text) # replace \n by <br> for plotly
df.Comments$Text = gsub("<br><br>$", "", df.Comments$Text) # replace \n by <br> for plotly
df.Comments$Text = gsub("<br><br>", "<br>", df.Comments$Text) # replace \n by <br> for plotly


g1 =ggplot(df.Comments) + 
	geom_smooth(aes(x = max(df.Comments$Week)-Week+1, y = Score))+
	geom_point(aes(x = max(df.Comments$Week)-Week+1, y = Score, color = "Score"), show.legend = FALSE)+
	theme_bw()+
	xlab("Week")+
	ylab("Score")+
	coord_cartesian(ylim = c(0,30))

	#ggplotly(g1)
	p1 <- plotly_build(g1)
	p1$data[[1]]$hoverinfo = "none"
	p1$data[[2]]$hoverinfo = "none"
	p1$data[[3]]$text = paste0(
		"Date: ",
		as.Date(df.Comments$Date),
		"<br>",
		
		"Score: ",
		df.Comments$Score,
		"<br>",
		
		"# of children: ",
		df.Comments$Children,
		"<br>",		
		
		"Text: ",
		df.Comments$Text
		)

	
g2 =ggplot(df.Comments) + 
	geom_smooth(aes(x = max(df.Comments$Week)-Week+1, y = Children))+
	geom_point(aes(x = max(df.Comments$Week)-Week+1, y = Children, label = Text, color = "Children"), show.legend = FALSE)+
	theme_bw()+
	xlab("Week")+
	ylab("Number of children comments")+
	coord_cartesian(ylim = c(0,20))
	
p2 <- plotly_build(g2)
	p2$data[[1]]$hoverinfo = "none"
	p2$data[[2]]$hoverinfo = "none"
	p2$data[[3]]$text = paste0(
		"Date: ",
		as.Date(df.Comments$Date),
		"<br>",
		
		"Score: ",
		df.Comments$Score,
		"<br>",
		
		"# of children: ",
		df.Comments$Children,
		"<br>",		
		
		"Text: ",
		df.Comments$Text
		)	
		
plotly_POST(p1, filename = "RedditRavioliScore")
plotly_POST(p2, filename = "RedditRavioliChildren")