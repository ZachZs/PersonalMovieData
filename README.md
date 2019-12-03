# PersonalMovieData

<p>Every year, I keep track of the movies I watch, including date, title, release year, and (sometimes) personal score. At the end of the year, I take the list and put it into an excel document of media habits I track, and set up the initial format of the final table.
<p>The two webscraping R files take the movie list and 1) scrap IMDB for the URL of the movie (if there is one), and 2) scrap the IMDB page for information such as director, IMDB score, etc. It is split into two files for convenience. By only getting the IMDB URLs in the first part, I can easily check for errors and correct the URLs before any significant scraping is done. Once all the URLs are correct, I can then go directly to that URL to get the data that I want.
<p>Once all of the data is collected, I can save the final table as a .csv file, and I can copy over the table to the media excel sheet that I have. [Note: there are several steps that could be done for efficiently in this process (such as saving as a .csv and copying over), but the data is usually less than 150 new rows each year, so it doesn't require too much extra work]
<p>The final R file is the R Shiny code to run it on a Shiny App. All of the visualizations and the datatable are constructed in this code. The app is also hosted on <a href="">shinyapps.io</a>. Included with the App is the current form of the dataset that I use to keep track of the media I watch. There are five tabs in the Excel document:
<ul>
 <li><em>Movies 2018</em>: This is the prior year's list that has been saved as a reference for formatting.</li>
 <li><em>Movie List (pre-R)</em>: This is the list of all the years of movies recorded, formatted into the final table format. This is the       page that will be uploaded into the R webscraping files.</li>
 <li><em>Movie List</em>: This is the final movie list after it has been run through the R webscraping files, and copied from the exported .csv back into this workbook. This is the final table that is used for the R Shiny file.</li>
 <li><em>Show List</em>: This is a list of (mostly) completed TV shows that I have watched. The format of the table is different than the movies list, so it was not run through any of the R code in this repository. It is only in the workbook for personal convenience. Maybe in the future I will make a similar R program for this list.</li>
 <li><em>Best Episodes</em>: This is a short list of my personal favorite epsiodes from the TV shows list. Again, this is here for personal convenience only, and it has no relation to the R files.</li>
</ul>
