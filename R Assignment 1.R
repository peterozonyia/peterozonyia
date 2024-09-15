# Creating a matrix of values between 1 and 10 
mat1<-c(1:10)

# Create a matrix of three rows and four columns
mat1 <- matrix(c(1,2,3,4,5,6,7,8,9,10), nrow = 3, ncol = 4)
print(mat1)

# Naming rows and columns using the rename function
rownames(mat1) = c("A","B","C")
colnames(mat1) = c("Q","W","E","R")

cat("The 3x4 matrix:\n")
print(mat1)

# Alternatively
mat1<-matrix(c(1,2,3,4,5,6,7,8,9,10), nrow = 3, ncol = 4,
             dimnames = list(c("A","B","C"),  c("Q","W","E","R")))
mat1


# Question 2 
# x = 24, y =”Hello World”, z = 93.65.
# Identify the class of x, y and z and convert all three into factor

# Converting vectors x, y, z into factor using assign function
x<-24
y<-"Hello World"
z<-93.65

myvect<-c(x,y,z)
print(myvect)

# using class function x is numeric, y is character, z is numeric
class(x)
class(y)
class(z)

# Question 3  q = 65.9836

# a. Find square root of q and round it up to 3 digits.
q<-65.9836
q
sqrt(q)
round(q, digits = 3)

# b Check if log to the base 10 of q is less than 2. 
log(65.9836, base=10) ## Output = 1.819436 < 2

# Question 4
# a. Find first 4 letters of each word in x

x<-c("Intelligence", "Knowledge", "Wisdom", "Comprehension")
substring(x, 1,4) # Extracting first four letters in x using substring()
y<-"I am"
z<-"intelligent"
paste(y,z) # combining y and z using paste() function
toupper(x) # converting all x words to upper case using toupper()

# Question 5. a = c(3,4,14,17,3,98,66,85,44)
# Print “Yes” if the numbers in ‘a’ are divisible by 3 and 
# “No” if they are not divisible by 3 using ifelse().

a <- c(3,4,14,17,3,98,66,95,44)
a

if(a % 3){
  print("a is divisible by 3")}



# Question 6. b = c(36,3,5,19,2,16,18,41,35,28,30,31)
# List all the numbers less than 30 in b using for loop.

b<-c(36,3,5,19,2,16,18,41,35,28,30,31)
b
new_b <- ifelse(b < 30, "yes", "no")
new_b


# 7. Date = “01/30/18”
# a) Convert Date into standard date format (yyyy-mm-dd) and 
  # name it as Date_new.
# b) Extract day of week and month from Date_new.
# c) Find the difference in the current system date and Date_new

Date <- "01/30/18"
date_new<-as.Date("01/30/18",format="%m/%d/%y")
date_new

weekdays(date_new)
months(date_new)

today<-Sys.Date()
today
