--import data into table

--create table to store data

CREATE TABLE "florInsur" (
	PolicyID INTEGER,
	State CHAR,
	County CHAR,
	eqsitelim INTEGER,
	husitelim INTEGER,
	flsitelim INTEGER,
	frsitelim INTEGER,
	tiv2011 INTEGER,
	tiv2012 INTEGER,
	eqsitededuct INTEGER,
	husitededuct INTEGER, 
	flsitededuct INTEGER,
	frsitededuct INTEGER,
	ptlat INTEGER,
	ptlong INTEGER,
	cline CHAR,
 	construct CHAR, 
	pointgran INTEGER 

);

--import data into table
--load in file
.mode csv
.import FL_insurance_sample.csv florInsur

--drop header row
DELETE FROM florInsur WHERE policyID ='policyID';

--Tell user how many observations displayed
.print "View first 10 observations"

--print first 10 observations from table
SELECT*FROM florInsur LIMIT 10;

--list unique counties in sample

.print "The counties found in the sample include"

--find unique counties and count total of each found
SELECT county, COUNT(*) FROM florInsur GROUP BY county;

--display average property appreciation YoY

.print "Property appreciated, on average by"

--calculate average property appreciation 
SELECT AVG(tiv2012-tiv2011) FROM florInsur;

--creating frequency table of construction materials
.print "Frequency of Construction Materials"

--The frequency of construction materials used
SELECT construct, COUNT(*) FROM florInsur GROUP BY construct;

