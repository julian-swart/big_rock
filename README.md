## Exploratory Data Analysis: The Big Rock Blue Marlin Fishing Tournament 

### Tournament context:

- The Big Rock is one of the largest competitive sportfishing tournaments in the world. It is held annually in the beginning of June and is based out of a small coastal town named Morehead City, North Carolina, which is about an hour from where I grew up. The ultimate goal of this tournament is to catch the heaviest Atlantic blue marlin, resulting in a large monetary prize of nearly 1 million dollars. 

- Participants can also compete in other categories, such as releasing the most billfish (blue marlin, white marlin, sailfish, and spearfish) or weighing in the heaviest meat fish (wahoo, tuna, mahi mahi). Terminology is further explained in the section below.

- The tournament is mainly focused on blue marlin, which can grow to weigh well over 1,000 pounds and measure up to 16 feet. The other species of billfish are much smaller and have to be released. 

- The entry fee this year was $25,000 to be entered into all categories (some boats don't enter into all categories).

- All categories include: 
  - Fabulous Fisherman: first blue marlin to be weighed over 500 lbs (this year's winner received $531,250).
  - Heaviest blue marlin (this year's winner caught a tournament record 914 lb fish and received $793,187).
  - Daily/aggregate billfish release: points are earned based on species (this year's aggregate winner received $142,375). 
  - Heaviest mahi mahi, wahoo and tuna (this year's winner for largest mahi mahi - also called a dolphin, not to be confused with Flipper - won $361,250).
  
- The tournament runs for 6 days. This year's dates were June 10th, 2019 (Monday) - June 15th, 2019 (Saturday).  Boats choose 4 of the 6 days to fish, usually picking days when the ocean won't be rough, if possible. 

- On Monday through Friday, boats are limited to fishing between 9AM and 3PM. On Saturday, boats are limited to fishing between 8AM and 2PM.

- The boats have to be within a certain boundary off of the North Carolina Coast. They typically fish 50 or more miles offshore around the Gulf Stream in hundreds to over a thousand feet of water.

- 184 sportfishing boats ranging from 26 to 97 feet long entered the tournament. At least 4 different states, 90 cities, and 70 different boat manufacturers were represented (57 boats did not have their port city or state listed on the website).

- Boats have to radio-in and report any activity, such as hooking-up to a fish, losing a fish, releasing a fish, or boating a fish. 

- A blue marlin must weigh at least 400 pounds or be at least 110 inches long in order to qualify. If the fish is brought to land and does not meet either of these criteria, that boat is penalized 400 points in the release division and 400 lbs on the next fish they land (if they land another one). This makes it very difficult to win any prize money. 

- The blue marlin must also be intact when weighed. This means if a shark were to take a bite of the fish as it is getting reeled in or if the propeller of the boat happened to cut the fish (both have happened), it would be disqualifed.  

- There are many other rules that we won't go into detail about here (including acceptable baits and supplies, every boat must leave from the same inlet, etc). 

- The tournament is run on the honor system, and winners have to pass a polygraph test to ensure no rules were broken.

- The prize money purse this year was a record high at $2.86 million! (The economy is doing well in 2019 apparently)

### Terminology explained:

  - The term *billfish* refers to the characteristic of a long spear-like nose/upper-jaw (a bill) that these fish have. They are fast, powerful, difficult to catch, fight extremely hard, and jump a lot when caught. These challenging and exciting characteristics, along with the size and beauty of these fish, are just a few reasons for the allure. Not to mention the enjoyment of being on the open ocean with the comforts of a deluxe sportfishing yacht. 

  - The term *meat fish* refers to the fish having edible and delicious meat. These fish have no bills and are typically smaller, more abundant, and easier to catch. However, they are still big and beautiful fish (normally 10-100 pounds). Billfish are generally not thought of as good to eat (although some people do smoke them).

  - The term *releasing* or *released* means the fish was let go instead of brought to land because it was too small, or because it wasn't a blue marlin.
  
  - The term *hooked-up* means a boat is actively fighting a fish that just ate one of the baits in the water.
  
  - The term *lost* means that the boat was fighting a fish, but the fish got off (it became un-hooked).
  
  - The term *wrong species* means the hooked-up fish that was caught or lost was not a qualifying species.
  
  - The term *boating*, *boated*, *land*, or *landed* means the crew of a boat was able to catch and bring aboard a blue marlin that they intend to weigh on land. The fish will only be boated if the crew believes it meets the 400 lb qualification (they make this decision by measuring its length and girth to calculate its approximate weight).

### Notes:

- Unfortunately, qualifying blue marlin are killed in order to be weighed on land. I don't have access to old data, but would guess an average of 7 are weighed each year. All carcasses are donated to NC State University's Center for Marine Sciences and Technology for research. 

- This tournament provides a major economic boost through increased tourism to an otherwise small, quiet beach town.

### Data exploration interests:

- As you can imagine, there are varying beliefs of what works best to attract and catch billfish, especially blue marlin. Different contributing factors include, but are not limited to: the type of lure, brand of lure, color of lure, bait type, water depth, water temperature, wind direction, current direction, geographical area, time of year, moon phase, sunny/cloudy skies, calm/choppy/rough seas, boat length, local knowledge, time of day, and even engine type (yes, some people believe that certain diesel engines create a unique hum or vibration in the water that attracts blue marlin better).

- Using the data available on the Big Rock website, I can provide insights into some of these variables using descriptive statistics, data visualizations, and correlations. If I had access to more data (like longitude and latitude of hook-ups, the lure that the fish ate, etc.), then I could complete an even more intriguing analysis using methods such as supervised machine learning to predict a winner, or unsupervised learning such as clustering to uncover valuable patterns. Sadly, fisherman keep data points like this very secret with hopes of having a competitive advantage. 

- With the data I was able to collect through web scaping, I want to explore:

  - The relationship of boat size and amount of billfish caught.

  - The relationship of boat brand and amount of billfish caught.

  - The relationship of the boat's port city/state and amount of billfish caught.

  - Do the above relationships differ if the fish was released or boated? 

  - Do the above relationships differ by the species of billfish? 

  - Distribution of time of day when billfish are caught.

  - Distribution and summary statistics for time fighting a fish.
  
  - How many boats did not catch anything? What is different about them? 

  - The businesses/job titles of the owners of the boats. I think it would be interesting to see how these people make a living. 

