//
// F# program to analyze payroll data.
//
// Subhra Kanungo
// U. of Illinois, Chicago
// CS 341, Spring 2020payroll_02.csv
// Project #02
//

#light
module Project02

open System.Net.Http.Headers
open System.Net.Http.Headers
open System.Net.NetworkInformation
open System.Reflection.Emit

let doubleOrNothing s = 
    match s with
    | "" -> 0.0
    | x -> float x

// First Name, Last Name, Occupation, Dept Name, Fulltime or Part time, Typical Hours, Annual Salary, Hourly Rate
let ParseCSVLine (line:string) = 
    let tokens = line.Split(',')
    let listOfValues = Array.toList tokens
    let fName::lName::Occupation::Department::SalaryType::HoursPerWeek::AnnualSalary::HourlyWage::[] = listOfValues
    (fName,lName,Occupation,Department,SalaryType,(doubleOrNothing HoursPerWeek),(doubleOrNothing AnnualSalary),(doubleOrNothing HourlyWage))

let rec ParseCSVDatabase lines = 
    let employees = Seq.map ParseCSVLine lines
    //printfn "%A" employees
    Seq.toList employees


//----------------------------------------------------------------------------------------------------------------------

let isSalary employee =
    let (fName,lName,Occupation,Department,SalaryType, HoursPerWeek, AnnualSalary, HourlyWage) = employee
    if(SalaryType = "Salary") then true else false
        
let isHourly employee =
    let (fName,lName,Occupation,Department,SalaryType, HoursPerWeek, AnnualSalary, HourlyWage) = employee
    if(SalaryType = "Hourly") then true else false

let getSalary employee =
    let (fName,lName,Occupation,Department,SalaryType, HoursPerWeek, AnnualSalary, HourlyWage) = employee
    AnnualSalary

let getDepartment employee =
    let (fName,lName,Occupation,Department,SalaryType, doubHoursPerWeek, AnnualSalary, HourlyWage) = employee
    Department

let getName employee =
    let (fName,lName,Occupation,Department,SalaryType, HoursPerWeek, AnnualSalary, HourlyWage ) = employee
    fName + " " + lName

//----------------------------------------------------------------------------------------------------------------------

let calcSalary employee =
    let (fName,lName,Occupation,Department,SalaryType, HoursPerWeek, AnnualSalary, HourlyWage) = employee
    if((isSalary employee) = false) then (HoursPerWeek * HourlyWage * 52.0) else AnnualSalary

//----------------------------------------------------------------------------------------------------------------------

// The function getNumberOfEmployees returns the number of employees in the data set.
let rec getNumberOfEmployees allData =
    match allData with
    | [] -> 0
    | hd ::[] -> 1
    | _::tl -> 1 + (getNumberOfEmployees tl) 
    
//----------------------------------------------------------------------------------------------------------------------

// The function getNumberOfSalariedEmployees returns the number of employees who have an annual salary in the data set.
let getNumberOfSalariedEmployees allData =
    
    let x = List.filter (fun x -> isSalary x = true) allData
    let rec helper x =
        match x with
        | [] -> 0
        | hd::tl -> 1+(helper tl)
    helper x
    
//----------------------------------------------------------------------------------------------------------------------

// The function getNumberOfSalariedEmployees returns the number of employees who are paid hourly in the data set.
let rec getNumberOfHourlyEmployees allData =
   let x = List.filter (fun x -> isHourly x = true) allData
   let rec helper x =
       match x with
       | [] -> 0
       | hd::tl -> 1+(helper tl)
   helper x

//----------------------------------------------------------------------------------------------------------------------

// The function findHighestPaidEmployee returns the name and salary
// of the highest paid employee.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployee allData =
    let rec helper newTup allData maxTup=
        match allData with
        | [] -> newTup
        | hd::tl when calcSalary maxTup <= calcSalary hd ->let name = getName hd
                                                           let salary = calcSalary hd
                                                           helper (name, salary) tl hd
        | _::tl -> helper newTup tl maxTup
    helper ("Nobody", 0.0) allData (List.head allData)
    
//----------------------------------------------------------------------------------------------------------------------

// The function findHighestPaidEmployee returns the salary
// of the highest paid employee within a specific department.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployeeInDept allData deptName =
    let filt = List.filter (fun x -> getDepartment x = deptName) allData
    let (_, salary) = findHighestPaidEmployee filt
    salary

//----------------------------------------------------------------------------------------------------------------------

// The function getAverageSalary calculates
// the average of the computed salary field.
let rec getAverageSalary allData =
    let salary = List.map (fun x -> calcSalary x) allData
    let allSalary = List.reduce (fun x y -> x + y) salary
    let totalEmployees = getNumberOfEmployees allData
    
    let helper x =
        match x with
        | 0 -> 0.0
        | x -> float x
    
    allSalary / (helper totalEmployees)
    
//----------------------------------------------------------------------------------------------------------------------

// The function getAverageSalary calculates 
// the average of the computed salary field for a specific department.
let rec getAverageSalaryInDept allData deptName =
    let filt = List.filter (fun x -> getDepartment x = deptName) allData
    getAverageSalary filt

//----------------------------------------------------------------------------------------------------------------------

// Searches through the data set to generate the list of all unique department names.
let getUniqueDepartmentNames allData =
    allData
    |> List.map getDepartment
    |> List.distinct
    |> List.sort
    
//----------------------------------------------------------------------------------------------------------------------

// The function howManyEmployeesInEachDepartment computes the number of employees in every department.  
// This function should return a list of tuples, pairs between the department name and number of employees.
let rec help L aName newTup count =
      match L with
      | [] -> newTup
      | _::tl -> help tl aName (aName, (count+1)) (count+1)
      
let rec _helper aName allData count =
    match allData with
    | [] -> ("No Department", 0)
    | (a,b,c,d,e,f,g,h)::tl when d = aName -> let x = List.filter (fun x -> getDepartment x = aName) allData
                                              help x d ("No Department", 0) 0
    | _::tl -> _helper aName tl count
    
let howManyEmployeesInEachDepartment allData deptNames =
    List.map(fun x -> _helper x allData 0) deptNames 
 
//----------------------------------------------------------------------------------------------------------------------

// The function findTotalSalaryByDepartment computes the overall annual salary budget for every department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a list of tuples, pairs between the department name and total annual salary.
let rec yhelp L aName newTup count =
      match L with
      | [] -> newTup
      | hd::tl -> let c = calcSalary hd
                  yhelp tl aName (aName, (count+c)) (count+c)
let rec _xhelper aName allData count=
    match allData with
    | [] -> ("No Department", 0.0)
    | (a,b,c,d,e,f,g,h)::tl when d = aName -> let x = List.filter (fun x -> getDepartment x = aName) allData
                                              yhelp x d ("No Department", 0.0) 0.0
    | _::tl -> _xhelper aName tl count
let findTotalSalaryByDepartment allData deptNames =
    List.map(fun x -> _xhelper x allData 0.0) deptNames 

//----------------------------------------------------------------------------------------------------------------------

// The function findHighestPaidDeptOverall returns the name and total annual salary
// of the department with the largest overall annual salary paid to employees in that department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a single tuple, containing the department name and total annual salary. 
let rec findHighestPaidDeptOverall allData deptNames =
    let deptList = findTotalSalaryByDepartment allData deptNames
    let x = List.head deptList
    
    let rec helper deptList maxTuple =
        match deptList, maxTuple with
        | [], _ -> maxTuple
        | (hd,hhd)::tl, (x,xs) when hhd > xs -> helper tl (hd, hhd)
        | _::tl, _ -> helper tl maxTuple
    helper deptList x
    
//----------------------------------------------------------------------------------------------------------------------

// The function withinSalaryRange returns the number of employees whose calculated salary
// is greater than the lower bound and less than or equal to the upper bound.
let rec withinSalaryRange lower upper L =
    match L with
    | [] -> 0
    | hd::tl when hd > lower && hd <= upper -> 1+(withinSalaryRange lower upper tl)
    | _::tl -> withinSalaryRange lower upper tl

//----------------------------------------------------------------------------------------------------------------------

// printOneHistogram
// prints one histogram value, the format is
//   label: *****value
// where the # of stars is value / amountPerStar.
let printOneHistogram label value amountPerStar =

  let rec printstars n = 
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printstars (n-1)
           
  printf " %16s: " label    // Enough space for all the departments in the file
  printstars (value / amountPerStar)
  printfn "%A" value
  
let rec table tupList amountPerStar =
    match tupList with
    | [] -> 0
    | (label,value)::tl -> printOneHistogram label value amountPerStar
                           table tl amountPerStar

//----------------------------------------------------------------------------------------------------------------------

let getALLSalary data =
    let x = List.map(fun xx -> calcSalary xx) data
    x
    
//----------------------------------------------------------------------------------------------------------------------

[<EntryPoint>]
let main argv =

  printfn "Enter name of the csv file containing employee data: "

  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let data = ParseCSVDatabase contents

  printf "Enter name of the department to be analyzed: "

  let deptname = System.Console.ReadLine()
  let allDepts = getUniqueDepartmentNames data
  
  let N = getNumberOfEmployees data
  printfn ""
  printfn "# of employees: %A" N
  printfn ""
  
  let deptNs = howManyEmployeesInEachDepartment data allDepts
  let (_,xt) = List.find(fun (name, _) -> name = deptname) deptNs
  printfn "# of employees in %s: %d" deptname xt      
  printfn ""
  
  //
  // % of employees salaried:
  //
  let numSalaried = getNumberOfSalariedEmployees data
  let percentSalaried = (double numSalaried) / (double N) * 100.0
  let numHourly = getNumberOfHourlyEmployees data
  let percentHourly = (double numHourly) / (double N) * 100.0
  
  printfn "%% of employees Salaried: %d (%.2f%%)" numSalaried percentSalaried
  printfn "%% of employees Hourly: %d (%.2f%%)" numHourly percentHourly
  printfn ""
  
  //
  // average salary:
  //
  let avgSalary = getAverageSalary data
  printfn "Average salary: %.2f" avgSalary
  //printfn ""
  
  //
  // average salary in department:
  //
  let avgSalary = getAverageSalaryInDept data deptname
  printfn "Average salary in %s: %.2f" deptname avgSalary
  printfn ""
  
  //
  // highest salary:
  //
  let (maxName,maxSalary) = findHighestPaidEmployee data
  printfn "Largest salary: %s paid %.2f annually" maxName maxSalary
  
  //
  // highest salary in department:
  //
  let maxSalary = findHighestPaidEmployeeInDept data deptname
  printfn "Largest salary in %s: %.2f" deptname maxSalary
  printfn ""
  
  printfn "** Histogram of employees by department (each star represents 5 employees):"    
  let allData = howManyEmployeesInEachDepartment data allDepts 
  let histTable = table allData 5
  printfn "%A" histTable
  printfn ""
  
  //
  // categorize salaries into 5 groups:
  //   0       < salary <= 60000
  //   60000   < salary <= 80000
  //   80000   < salary <= 100000
  //   100000  < salary <= 120000
  //   120000  < salary <= 10,000,000  // arbitrary upper bound to reuse function
  //
  
  let allSalary = getALLSalary data
  let allEmployees = getNumberOfEmployees data
  let sumx = List.sum allSalary
  
  let count60korless = withinSalaryRange (float 0) (float 60000.0) allSalary
  let percent60korless = (float count60korless) / (float allEmployees) * 100.0

  let count60kto80k = withinSalaryRange (float 60000) (float 80000) allSalary
  let percent60kto80k = (float count60kto80k) / (float allEmployees) * 100.0

  let count80kto100k = withinSalaryRange (float 80000) (float 100000) allSalary
  let percent80kto100k = (float count80kto100k) / (float allEmployees) * 100.0

  let count100kto120k = withinSalaryRange (float 100000) 120000.0 allSalary
  let percent100kto120k = (float count100kto120k) / (float allEmployees) * 100.0

  let countgreater120k = withinSalaryRange (float 120000) (float 1000000) allSalary
  let percentgreater120k = (float countgreater120k) / (float allEmployees) * 100.0

  printfn "** Salary Ranges:"
  printfn " 0-60000 : %A (%.2f%%)" count60korless percent60korless
  printfn " 60000-80000 : %A (%.2f%%)" count60kto80k percent60kto80k
  printfn " 80000-100000: %A (%.2f%%)" count80kto100k percent80kto100k
  printfn " 100000-120000: %A (%.2f%%)" count100kto120k percent100kto120k
  printfn " > 120000: %A (%.2f%%)" countgreater120k percentgreater120k
  printfn ""

  
  printfn "** Histogram of Salary Ranges (each star represents 10 employees):"    
  let salaryGroups = [("<60000", count60korless);("60-80k", count60kto80k);("80-100k", count80kto100k);("100-120k", count100kto120k);(">120000",countgreater120k)]
    // Use printOneHistogram to build the histogram
  let salaryTable = table salaryGroups 10
  printf "%A" salaryTable
  printfn ""
  
  0
