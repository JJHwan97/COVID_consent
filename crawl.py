from selenium import webdriver
from selenium.webdriver.support.ui import Select
import pandas as pd
from selenium.webdriver.common.keys import Keys
import pyautogui
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
import time

data = pd.read_csv("C:\\Users\\joshu\\Desktop\\python\\location2.csv",encoding = 'cp949')
data = pd.DataFrame(data)
data.rename(columns=data.iloc[0])

date = pd.read_csv("C:\\Users\\joshu\\Desktop\\python\\date.csv",encoding = 'utf-8')
date = pd.DataFrame(date)
date = date.applymap(str)

# Chrome의 경우 | 아까 받은 chromedriver의 위치를 지정해준다.
driver = webdriver.Chrome()
# PhantomJS의 경우 | 아까 받은 PhantomJS의 위치를 지정해준다.
# driver = webdriver.PhantomJS('/Users/beomi/Downloads/phantomjs-2.1.1-macosx/bin/phantomjs')
driver.implicitly_wait(3)
# url에 접근한다.
driver.get('https://www.stcis.go.kr/pivotIndi/wpsPivotIndicator.do?siteGb=P&indiClss=IC01')

driver.maximize_window()

element = driver.find_element_by_link_text("목적통행량")
#element =driver.find_element_by_xpath("//a[@href='javascript:app.panel.rowFields.clear();app.panel.columnFields.clear();selectIndiSel('IC0102');' and text()='목적통행량']")
element.click();
time.sleep(5) 
element = driver.find_element_by_id("searchAreaGubun2")
element.click();

i = 0

while i < 17:
    city = data.loc[i,"sdNm"]
    #district = data.loc[i,"sggNm"]
    dropdown1 = Select(driver.find_element_by_id("searchZoneSd"))
    dropdown1.select_by_visible_text(locals()['city'])
    #dropdown1 = Select(driver.find_element_by_id("searchZoneSgg"))
    #dropdown1.select_by_visible_text(locals()['district'])
    #driver.find_element_by_id("chkZoneEmd").click()
    #element = driver.find_element_by_id("chkZoneEmd")
    #driver.execute_script("arguments[0].click();", element)
    #driver.implicitly_wait(10)
    #pyautogui.click(42,613)
    #driver.find_element_by_link_text("읍/면/동 전체선택").click()
    #element = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, "idnachkZoneEmdme")))
    #element.click()
    #element = WebDriverWait(driver, 10).until(
    #    EC.presence_of_element_located((By.ID, "chkZoneEmd"))
    #)
    #element.click()
    time.sleep(10) 
    driver.find_element_by_id("chkZoneSgg").click()
    #wait = WebDriverWait(driver, 10)
    #element = wait.until(EC.element_to_be_clickable((By.ID, 'chkZoneEmd')))

    driver.find_element_by_xpath("//button[@onclick=\"fnAreaSel();return false;\"]").click()
    i = i+1

# Make Date

#dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-year"))
#dropdown1.select_by_visible_text('2019')

#dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-month"))
#dropdown1.select_by_visible_text('1월')

#element = driver.find_element_by_link_text("5")
#element.click();
    




#dropdown1 = Select(driver.find_element_by_id("searchZoneSd"))
#dropdown1.select_by_visible_text('서울특별시')

#dropdown1 = Select(driver.find_element_by_id("searchZoneSgg"))
#dropdown1.select_by_visible_text('강남구')

#element = driver.find_element_by_id("chkZoneEmd")
#element.click();

#driver.find_element_by_xpath("//button[@onclick=\"fnAreaSel();return false;\"]").click()

#driver.find_element_by_xpath("//button[@onclick=\"fnSearch();return false;\"]").click()

#element = driver.find_element_by_id("FINEDUST")
#element.click();
#driver.find_element_by_name('pw').send_keys('mypassword1234')

element = driver.find_element_by_class_name("searchDayTo")
element.click();

dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-year"))
dropdown1.select_by_visible_text("2019")

dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-month"))
dropdown1.select_by_visible_text("1월")

element = driver.find_element_by_link_text("1")
element.click();
#Search
driver.find_element_by_xpath("//button[@onclick=\"fnSearch();return false;\"]").click() 

#Download
time.sleep(45) 
#driver.find_element_by_id("btnExport").click()

#back2search
#driver.find_elements_by_xpath("//a[contains(text(), '조회')]")
#driver.find_element_with_partial_link("조회")
#driver.find_element_by_class_name("inquiry_tab").click
time.sleep(15) 
#pyautogui.click(x=89, y=221)
# 큰화면
pyautogui.click(x=72, y=174)

i=517   

while i < 547:
    time.sleep(3) 
    year = date.loc[i,"Year"]
    month = date.loc[i,"Month"]
    day = date.loc[i,"Day"]

    element = driver.find_element_by_class_name("searchDayTo")
    element.click();
    
    dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-year"))
    dropdown1.select_by_visible_text(locals()['year'])

    dropdown1 = Select(driver.find_element_by_class_name("ui-datepicker-month"))
    dropdown1.select_by_visible_text(locals()['month'])

    element = driver.find_element_by_link_text(locals()['day'])
    element.click();

    driver.find_element_by_xpath("//button[@onclick=\"fnSearch();return false;\"]").click()

    time.sleep(45) 

    driver.find_element_by_id("btnExport").click()
    
    time.sleep(45) 

    pyautogui.click(x=72, y=174)

    i = i + 1

