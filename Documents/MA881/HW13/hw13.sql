# Cars with 1 or 2 carburetors and their gas mileages 
SELECT mpg, carb FROM mtcars
WHERE carb = 1 OR carb = 2;

# Cars not with 1 or 2 carburetors and their gas mileages
SELECT mpg, carb FROM mtcars
WHERE carb != 1 AND carb != 2;

/* Is the gas mileage of the cars with 1 or 2 carburetors different from 
the gas mileage of the other cars in the list?
Yes, 
I perform a hypothesis test to test if the gas mileage of the cars with 1 or 2 carburetors 
is different from the gas mileage of the other cars
I get the p-value = 9.218e-05 < 0.05, so we can reject the above H0.
Thus, there is a significant difference between the mean gas mileage of the cars with 
1 or 2 carburetors and the mean gas mileage of the other cars.*/

# Gas mileage of “sporty” cars 
SELECT mtcars.mpg, survey.Sporty
FROM mtcars INNER JOIN survey
ON mtcars.car=survey.car AND mtcars.ID=survey.ID 
WHERE survey.Sporty=1;

# Gas mileage of not “sporty” cars 
SELECT mtcars.mpg, survey.Sporty
FROM mtcars INNER JOIN survey
ON mtcars.car=survey.car AND mtcars.ID=survey.ID 
WHERE survey.Sporty=0;

/*Do “sporty” cars get better gas mileage than the other cars in mtcars?
No, 
I export the results and perform a hypothesis test in R to test if “sporty” cars 
get better gas mileage than the other cars in mtcars.
I get the p-value = 0.08401 > 0.05, so we fail to reject the above H0.
Thus, there is not a significant difference between the mean gas mileage 
of “sporty” cars and the mean gas mileage of the other cars in mtcars.*/
