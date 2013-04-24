val testIsOlder1 = is_older((3, 1, 1993), (6, 25, 2000)) = true;
val testIsOlder2 = is_older((3, 1, 1993), (6, 25, 1990)) = false;
val testIsOlder3 = is_older((1, 1, 1993), (1, 1, 1993)) = false;

val testNumberInMonth1 = number_in_month([(3, 1, 1993), (6, 25, 2000), (3, 4, 1900)], 3) = 2;

val testNumberInMonths1 = number_in_months([(3, 1, 1993), (6, 25, 2000), (3, 4, 1900)], [3, 6]) = 3;

val testDatesInMonth = dates_in_month([(3, 1, 1993), (6, 25, 2000), (3, 4, 1900)], 5) = [];
val testDatesInMonth1 = dates_in_month([(3, 1, 1993), (5, 25, 2000), (5, 4, 1900)], 5) = [(5, 25, 2000), (5, 4, 1900)];

val testDatesInMonths1 = dates_in_months([(3, 1, 1993), (5, 25, 2000), (5, 4, 1900),(12,19,2001)],[3,5]) = [(3,1,1993),(5, 25, 2000), (5, 4, 1900)];

val testGetNth1 = get_nth(["hi","class","coursera","is","awesome"],3)="coursera";
val testGetNth2 = get_nth(["hi","class","coursera","is","awesome"],1)="hi";
val testGetNth3 = get_nth(["hi","class","coursera","is","awesome"],5)="awesome";

val testDateToString1 = date_to_string((1,1,1993)) = "January 1, 1993";
val testDateToString2 = date_to_string((2,29,1992)) = "February 29, 1992";

val testNumBeforeReachingSum1 = number_before_reaching_sum(11, [1,2,3,4,5,6]) = 4;
val testNumBeforeReachingSum2 = number_before_reaching_sum(1, [1,2,3,4,5,6]) = 0;
val testNumBeforeReachingSum3 = number_before_reaching_sum(21, [1,2,3,4,5,6]) = 5;

val testWhatMonth1 = what_month(5) = 1;
val testWhatMonth2 = what_month(32) = 2;
val testWhatMonth3 = what_month(60) = 3;

val testMonthRange1 = month_range(30, 34) = [1,1,2,2,2];

val testOldest1 = oldest([(3, 1, 1993), (5, 25, 2000), (5, 4, 1900)]) = SOME(5,4,1900);
val testOldest2 = oldest([(3, 1, 1993), (5, 25, 2000), (5, 4, 1999)]) = SOME(3,1,1993);

val testReasonableDate1 = reasonable_date((2,29,1992)) = true;
val testReasonableDate2 = reasonable_date((2,29,1993)) = false;
val testReasonableDate3 = reasonable_date((12,31,1233)) = true;
val testReasonableDate4 = reasonable_date((13,299,~1992)) = false;

