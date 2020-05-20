import ssl
import re
ssl._create_default_https_context = ssl._create_unverified_context
from urllib.request import urlopen
from bs4 import BeautifulSoup
import xlsxwriter


birkie_page1 = []
birkie_skiers = []


for aa in range(0,39):

	birkie_page0 = "http://birkie.pttiming.com/results/2020/index.php?pageNum_rsOverall="+str(aa)+"&totalRows_rsOverall=3862&page=1150&r_page=division&divID=1"
	birkie_page0 = urlopen(birkie_page0)
	birkie_soup0 = BeautifulSoup(birkie_page0, 'html.parser')
	ind=0



	for a in birkie_soup0.find_all('a'):
		if(ind>9 and ind<210):
			if(ind%2==1):
				#print(str(a.get('href')))
				#print((ind-8)/2, a.get('href'))
				if(str(a.get('href')).startswith("?page=1150")==True):
					birkie_page1.append("http://birkie.pttiming.com/results/2020/index.php"+a.get('href'))
				else:
					break

		ind+=1
		
		if(ind>210):
			break

#print(birkie_page1)

for a in range(len(birkie_page1)):
	if(a%100==0):
		print(a)
	
#for a in range(10):
	birkie_soup1 = BeautifulSoup(urlopen(birkie_page1[a]), 'html.parser')
	body = birkie_soup1.body.find_all('table')
	name = body[0]
	name = (name.find('h4'))
	name = name.text
	demos = (body[1])
	demos = (demos.find_all("tr")[0])
	bib = (demos.find_all("tr")[2])
	bib = bib.find_all("td")
	bib = (bib[1].text)
	age = demos.find_all("tr")[3]
	age = age.find_all("td")
	age = age[1].text
	sex = demos.find_all("tr")[4]
	sex = sex.find_all("td")
	sex = sex[1].text
	
	
	times = body[4]
	starttime = str(times.find_all("td")[3].text).strip()
	#print(starttime)
	dist1 = str(times.find_all("td")[9].text).split(" ")[0].strip()
	
	firetower = times.find_all("td")[10].text.strip()
	dist2 = str(times.find_all("td")[17].text).split(" ")[0].strip()
	oo = times.find_all("td")[18].text.strip()
	

	try:
		dist3 = str(times.find_all("td")[25].text).split(" ")[0].strip()
		mosquito_brook = times.find_all("td")[26].text.strip()
	except:
		dist3 = "NA"
		mosquito_brook = "NA"
	try:
		dist4=str(times.find_all("td")[33].text).split(" ")[0].strip()
		lake_hayward = times.find_all("td")[34].text.strip()
	except:
		dist4 = "NA"
		lake_hayward = "NA"

	try:
		dist5 = str(times.find_all("td")[41].text).split(" ")[0].strip()
		finish = times.find_all("td")[42].text.strip()
	except:
		dist5 = "NA"
		finish = "NA"
	name = name.strip()
	bib = bib.strip()
	age = age.strip()
	sex = sex.strip()
	skier = [name, bib, age, sex, starttime, dist1, firetower, dist2, oo, dist3, mosquito_brook, dist4, lake_hayward, dist5, finish]
	birkie_skiers.append(skier)



print(len(birkie_skiers))
workbook = xlsxwriter.Workbook("/Users/syverjohansen/ski/birkie/birke.xlsx")
birkie = workbook.add_worksheet("Birkie")
row = 0
col = 0
for a in range(len(birkie_skiers)):	
	birkie.write(row, col, birkie_skiers[a][0])
	birkie.write(row, col+1, birkie_skiers[a][1])
	birkie.write(row, col+2, birkie_skiers[a][2])
	birkie.write(row, col+3, birkie_skiers[a][3])
	birkie.write(row, col+4, birkie_skiers[a][4])
	birkie.write(row, col+5, birkie_skiers[a][5])
	birkie.write(row, col+6, birkie_skiers[a][6])
	birkie.write(row, col+7, birkie_skiers[a][7])
	birkie.write(row, col+8, birkie_skiers[a][8])
	birkie.write(row, col+9, birkie_skiers[a][9])
	birkie.write(row, col+10, birkie_skiers[a][10])
	birkie.write(row, col+11, birkie_skiers[a][11])
	birkie.write(row, col+12, birkie_skiers[a][12])
	birkie.write(row, col+13, birkie_skiers[a][13])
	birkie.write(row, col+14, birkie_skiers[a][14])
	row+=1

workbook.close()







#tempbirksoup = (birkie_soup0.find_all("tr"))
#dyr = (tempbirksoup[9])
#dyr_link = dyr.find_all('a', href=True)
#print(dyr_link)


#for b in birkie_soup0.find_all('a', {'class':'ablue'}, href = True):
#	men_worldcup_page1.append('https://skisport365.com/ski/'+b['href'])