library(RSelenium)
library(rvest)

remDr<- remoteDriver(remoteServerAddr= "localhost",
                     port= 4445L,
                     browserName= "chrome")

remDr$open()
remDr$navigate("https://data.oecd.org/agrland/nutrient-balance.htm#indicator-chart")
table_bt<- remDr$findElement("css selector", "li:last-child.chart-types-item a.chart-types-button")
table_bt$clickElement()

page_source<- remDr$getPageSource()

# 멀티 테이블인 경우와 단일 테이블인 경우

# 단일 테이블인 경우

remDr$navigate("https://data.oecd.org/agrland/agricultural-land.htm#indicator-chart")
table_bt<- remDr$findElement("css selector", "li:last-child.chart-types-item a.chart-types-button")
table_bt$clickElement()

<- read_html(page_source[[1]]) %>% html_nodes(".table-chart-value") %>% html_text # population

pp


##

GDP per hour worked
Labour productivity forecast
Labour productivity and utilisation
Labour compensation per hour worked
Multifactor productivity
Unit labour costs
Value-added in non-financial corporations
Financial corporations debt to equity ratio
Non-Financial corporations debt to surplus ratio
Banking sector leverage
Household disposable income
Household spending
Household savings
Household savings forecast
Household debt                            000
Household financial assets
Household financial transactions
Household net worth
Students per teaching staff               000
General government deficit
General government revenue
General government spending
General government spending by destination
General government debt
General government financial wealth
Government production costs
Government reserves
Central government spending 000
Tax revenue                000
Tax on personal income          000
Tax on corporate profits        000
Social security contributions
Tax on payroll
Tax on property
Tax on goods and services
Tax wedge
Harmonised unemployment rate (HUR)
Unemployment rate                       000
Unemployment rate forecast
Unemployment rates by education level
Long-term unemployment rate
Youth unemployment rate                   000
Road accidents                           000
Passenger transport
Freight transport
Container transport
Passenger car registrations
Infrastructure investment
Infrastructure maintenance
Income inequality                   000
Poverty rate
Poverty gap
Discriminatory family code
Violence against women
Women in politics
Social Institutions and Gender
Population                   000
Working age population
Young population                   000
Elderly population
Fertility rates                    000
Permanent immigrant inflows          000
Foreign-born population
Foreign population
Native-born employment
Foreign-born employment
Native-born unemployment
Foreign-born unemployment
Native-born participation rates
Foreign-born participation rates
Stocks of foreign-born population in OECD countries
Social spending                                     000
Pension spending
Public unemployment spending
Family benefits public spending
Social benefits to households
Public spending on incapacity
Public spending on labour markets
Mobile broadband subscriptions
Fixed broadband subscriptions
Business use of broadband
Households with broadband access         000
Gross domestic product (GDP) per capita           000
Quarterly GDP
Real GDP forecast
Nominal GDP forecast
Real GDP long-term forecast
Investment (GFCF)
Investment forecast
Investment by sector
Investment by asset
Domestic demand forecast