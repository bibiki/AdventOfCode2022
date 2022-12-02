% X|Xs is a list of number with random values 'void' in between. the numbers between two voids or between the beginning (end) of list and a void identify calories of foods a particular elf is carrying. X|Xs could looke like a list of lists [[1, 2, 3], [6, 7, 8], [elf3], ...], but looks like [1, 2, 3, void, 6, 7, 8, void, elf3..]
%calorie sums calculates the totals of each inner list in the input list
calorie_sums([X|Xs], Sum, Sums) :- integer(X), Sum1 is Sum + X, calorie_sums(Xs, Sum1, Sums).
calorie_sums([X|Xs], Sum, [Sum|Sums]) :- \+ integer(X), calorie_sums(Xs, 0, Sums).
calorie_sums([], S, [S]).

% finds max in a given list of numbers
max([X, Y], X) :- X >= Y.
max([X, Y], Y) :- Y > X.
max([X, Y|Xs], Z) :- X >= Y, max([X|Xs], Z).
max([X, Y|Xs], Z) :- X < Y, max([Y|Xs], Z).

% merges two lists maintaining order; auxiliary predicate to help with merge_sort
merge([], R, R).
merge(L, [], L).
merge([El|L], [Er|R], [El|M]) :- El < Er, merge(L, [Er|R], M).
merge([El|L], [Er|R], [Er|M]) :- El >= Er, merge([El|L], R, M).

% merge_sorts does merge sort does merge sort does m...
merge_sort([X], [X]).
merge_sort([X, Y], [X, Y]) :- X =< Y.
merge_sort([X, Y], [Y, X]) :- X > Y.
merge_sort(Xs, Ys) :-
    length(Xs, L),
    Half is L//2,
    append(Left, Right, Xs),
    length(Left, Half),
    merge_sort(Left, Lsorted),
    merge_sort(Right, Rsorted),
    merge(Lsorted, Rsorted, Ys).

% in a list of more than 2 elements, this identifies the last three
last_three([A, B, C], [A, B, C]).
last_three([A, B, C, D | M], [Aa, Bb, Cc]) :- last_three([B, C, D | M], [Aa, Bb, Cc]).

% advent of code day 1 solutions
solution1(Result) :- input(X), calorie_sums(X, 0, S), max(S, Result).
solution2(Result) :- input(X), calorie_sums(X, 0, S), merge_sort(S, Sorted), last_three(Sorted, [A, B, C]), Result is A + B + C.

input([8462,
6981,
3714,
4409,
8186,
3614,
2218,
7558,
6702,
void,
2947,
4727,
6396,
5718,
2361,
1970,
1583,
2816,
2995,
6914,
4313,
1401,
void,
1643,
7815,
2162,
8841,
7671,
void,
5740,
5471,
4838,
2281,
2784,
4587,
1497,
2074,
1161,
4388,
2233,
4795,
2011,
5438,
void,
14064,
9646,
2537,
void,
2064,
2833,
1164,
14379,
void,
6008,
4327,
4709,
5412,
2745,
5680,
5322,
4375,
4064,
4822,
6523,
6824,
6822,
void,
12100,
24791,
18239,
void,
9704,
8218,
9949,
3078,
9407,
3732,
4281,
6727,
void,
9833,
2549,
5042,
7053,
6116,
9624,
5393,
8359,
void,
4195,
7869,
15434,
6997,
5776,
void,
2487,
7843,
2739,
5227,
4088,
6281,
7078,
4291,
5461,
4838,
7936,
void,
5449,
5754,
4685,
7642,
7244,
4912,
void,
4020,
18761,
17710,
8736,
void,
6204,
1295,
3812,
5078,
4627,
3818,
4351,
5202,
2542,
2728,
3036,
5026,
5146,
5771,
void,
8809,
5364,
22076,
void,
3846,
2867,
10260,
3321,
7715,
7670,
1326,
5480,
void,
2613,
1442,
3771,
1692,
6131,
3945,
4591,
5165,
1648,
6084,
4952,
4295,
4390,
3935,
void,
4403,
6299,
4307,
5329,
4235,
4779,
4498,
6871,
5739,
4102,
6815,
1975,
4056,
void,
3875,
2650,
5888,
1605,
3972,
4781,
5694,
4150,
5765,
3919,
4260,
6339,
5614,
4597,
void,
1829,
4292,
4756,
4348,
3051,
2662,
4018,
1498,
3391,
1776,
1919,
3141,
3938,
void,
17338,
10081,
22228,
void,
10554,
2454,
2879,
3605,
3717,
5477,
4826,
3133,
void,
2221,
1431,
3235,
3371,
1364,
2137,
1774,
2015,
5115,
3331,
3588,
3264,
2401,
void,
5770,
3408,
1153,
6106,
3261,
2801,
3380,
1377,
2126,
5516,
4087,
2353,
1039,
1909,
5374,
void,
15655,
12356,
7421,
12094,
1776,
void,
16080,
7634,
4329,
void,
9409,
10153,
10744,
2145,
5796,
1586,
3622,
3635,
void,
1314,
5713,
5566,
6397,
2033,
1083,
7096,
6595,
6652,
4872,
4444,
void,
9731,
12016,
10148,
7358,
10025,
7471,
1346,
void,
1189,
2777,
4153,
1415,
9311,
6324,
4108,
7104,
8467,
void,
9326,
3339,
5048,
8225,
1647,
3640,
2922,
void,
3110,
15978,
17916,
void,
4502,
4106,
5683,
8061,
6507,
3591,
4560,
4620,
7036,
5762,
1470,
void,
7613,
12487,
1257,
10687,
4374,
void,
14169,
1170,
void,
2310,
10184,
6759,
12955,
3895,
6388,
void,
19637,
11418,
4728,
10935,
void,
8856,
9067,
7661,
3795,
3895,
7974,
2340,
3395,
1442,
void,
4258,
3584,
1935,
2919,
3870,
1078,
4163,
5192,
3363,
2386,
5897,
2922,
1619,
4872,
1903,
void,
8638,
8492,
1883,
10979,
2715,
3274,
7286,
void,
10670,
13042,
13137,
void,
3729,
1312,
1981,
2754,
4417,
1466,
7675,
2290,
5074,
5666,
void,
13836,
1263,
24451,
void,
6390,
8913,
12614,
6468,
2541,
13763,
void,
4673,
7245,
9128,
4712,
3838,
4596,
6620,
5459,
6350,
void,
4454,
5111,
3862,
1374,
2091,
2522,
6196,
2052,
1399,
6151,
1987,
3678,
void,
7427,
6633,
1447,
1854,
6601,
3459,
4633,
1339,
2955,
3929,
1057,
1910,
void,
1896,
2326,
2173,
1062,
5249,
3730,
4479,
6377,
2833,
4106,
4126,
4492,
2132,
4484,
void,
3465,
5345,
2295,
5946,
2465,
5684,
5573,
4449,
5983,
4863,
3678,
6007,
1249,
1563,
5270,
void,
3334,
1612,
3210,
1215,
4474,
2893,
4987,
2355,
3912,
5019,
4544,
1446,
3076,
4833,
4889,
void,
8299,
3042,
5715,
8442,
10294,
3697,
2071,
1482,
void,
2461,
6281,
3382,
1175,
7048,
2050,
1470,
5249,
7622,
6950,
4962,
void,
8573,
6266,
12922,
11159,
1493,
void,
6320,
4365,
7883,
void,
17708,
18169,
11780,
void,
8023,
7870,
4647,
6059,
1094,
1888,
8696,
2077,
5882,
3320,
void,
3180,
5779,
2360,
6811,
7802,
1662,
3157,
7373,
4282,
5282,
void,
3162,
5081,
1917,
1149,
2325,
6862,
4536,
2776,
6418,
4161,
6919,
1766,
2387,
void,
11508,
6617,
1521,
6854,
4799,
1463,
11379,
void,
12419,
4634,
8522,
3323,
void,
43380,
void,
1529,
2302,
6234,
5285,
6071,
3741,
6783,
3271,
1451,
3379,
1032,
1218,
void,
53976,
void,
3746,
2269,
4878,
5085,
1873,
3582,
2155,
6638,
2804,
3910,
5384,
4865,
void,
4905,
2648,
2320,
1775,
3803,
1137,
2405,
1974,
1667,
5677,
3687,
2476,
5332,
void,
2511,
2805,
3088,
5437,
4000,
5283,
6220,
3387,
1962,
2868,
2084,
5740,
4899,
5846,
void,
20975,
15672,
void,
11633,
12928,
17372,
void,
10964,
6326,
3115,
18901,
void,
2151,
6218,
2619,
8294,
12292,
12179,
void,
5390,
4373,
3685,
2236,
7107,
11917,
3701,
void,
5696,
1751,
6613,
6606,
1288,
5362,
6157,
6236,
3649,
4023,
void,
5290,
7999,
10040,
3338,
7547,
7575,
6369,
void,
3358,
6414,
2066,
2012,
3849,
1421,
2962,
4607,
3548,
6637,
5759,
4800,
5974,
void,
3209,
11201,
10317,
7340,
1073,
11610,
void,
5148,
2081,
5577,
5340,
7634,
4998,
7107,
6923,
1673,
void,
4809,
6274,
4140,
2993,
3003,
2973,
9914,
1878,
void,
3190,
5792,
3561,
2917,
4408,
1443,
4373,
1241,
3288,
5703,
4155,
5764,
4857,
5828,
4089,
void,
7390,
5924,
7987,
2919,
4474,
7700,
5457,
7926,
7539,
void,
5344,
5739,
4883,
1156,
5896,
5991,
7625,
2118,
3292,
7562,
1820,
void,
6485,
7804,
6886,
7436,
6163,
2618,
3119,
7144,
6274,
2792,
7602,
void,
4912,
6024,
5984,
2408,
1685,
1702,
5851,
2756,
1341,
4558,
1073,
4394,
6014,
1395,
void,
6662,
6573,
6043,
7402,
5790,
6942,
4171,
4043,
3077,
5672,
4984,
void,
1292,
6545,
2265,
6121,
2385,
2229,
6870,
6579,
1549,
6030,
3423,
1830,
6027,
void,
6361,
5979,
6532,
7110,
1881,
4659,
void,
9825,
4176,
4534,
1732,
4816,
6604,
8182,
2391,
void,
2309,
5266,
5879,
2463,
3636,
4958,
6308,
2091,
2588,
1841,
6899,
5666,
void,
9351,
void,
3824,
5754,
2710,
3782,
2784,
4535,
6157,
1055,
3569,
6498,
2679,
2949,
3285,
2519,
void,
5661,
void,
4565,
8056,
1975,
6947,
7508,
5570,
3727,
3287,
1794,
4933,
6458,
void,
6171,
9991,
10533,
6503,
1450,
void,
18391,
9046,
17384,
18940,
void,
5583,
5102,
2869,
5328,
2041,
4327,
1656,
2874,
2811,
5765,
4568,
5280,
2770,
1147,
6082,
void,
1512,
5928,
8631,
3263,
5569,
3693,
5060,
1810,
8327,
2900,
void,
21146,
13775,
13189,
void,
2853,
1782,
1461,
2560,
2046,
2773,
2453,
1218,
3545,
6013,
5893,
1853,
1926,
1847,
5914,
void,
15312,
22899,
void,
2752,
1253,
1448,
2627,
1161,
3661,
3457,
2992,
4606,
3206,
3145,
3639,
2833,
3687,
2148,
void,
6668,
3671,
3484,
2000,
2149,
3748,
1256,
3168,
6889,
5046,
6594,
void,
7437,
5596,
6744,
6863,
7397,
5922,
1897,
4042,
3993,
2569,
2509,
6048,
void,
10456,
7724,
2668,
4749,
void,
11329,
5011,
4007,
10189,
6972,
void,
4514,
1702,
2925,
1828,
8000,
5724,
3567,
1190,
void,
2949,
7487,
6652,
1472,
5965,
3071,
6990,
3356,
5283,
6487,
1990,
void,
31016,
void,
3585,
6168,
6826,
6184,
4985,
6209,
7290,
5732,
4058,
7919,
2152,
void,
18469,
void,
5945,
4318,
3986,
3386,
7037,
7149,
5313,
6521,
8266,
6665,
void,
39430,
void,
2779,
2507,
4473,
1635,
6487,
6823,
5443,
7274,
4722,
6267,
6421,
void,
8443,
3747,
3863,
6109,
1369,
8602,
4231,
7029,
4102,
2876,
void,
1013,
2319,
1800,
5256,
9521,
12068,
2116,
void,
11032,
1733,
6220,
7666,
3891,
3761,
6096,
void,
3434,
4984,
3662,
3070,
3560,
4494,
1485,
3460,
3851,
1007,
2615,
5349,
4254,
5248,
2140,
void,
3469,
2318,
4553,
1058,
1122,
4413,
3672,
6019,
1616,
3557,
3211,
2938,
4390,
2685,
3250,
void,
1446,
5593,
4857,
2389,
6242,
2724,
1425,
7948,
7140,
7839,
6895,
void,
5659,
4475,
5467,
1742,
1243,
8125,
2044,
3449,
5796,
void,
3012,
12758,
2478,
11102,
2615,
7856,
void,
5170,
5356,
1141,
1513,
1650,
4280,
2441,
4572,
6556,
6821,
3519,
3330,
void,
4799,
6959,
1737,
1502,
3510,
2089,
6946,
1303,
3574,
6382,
void,
6579,
2997,
1700,
6006,
1855,
2135,
10659,
2237,
void,
55371,
void,
1551,
2828,
3490,
2030,
5126,
4109,
2305,
4994,
4733,
1693,
3338,
3996,
4181,
3896,
1092,
void,
1846,
4756,
9555,
7994,
8733,
1489,
4585,
5954,
6058,
void,
48724,
void,
3025,
6315,
1398,
2563,
2661,
6414,
1230,
1290,
1898,
1493,
1347,
3971,
4248,
void,
8315,
17843,
17103,
void,
4287,
2732,
2284,
3036,
6831,
2890,
7588,
7268,
1464,
2247,
1306,
void,
4480,
1267,
5209,
3558,
5311,
2958,
3054,
4265,
3836,
3300,
1867,
1892,
5756,
3293,
void,
7569,
9154,
4238,
1586,
7972,
9683,
5373,
3431,
4990,
void,
9579,
6370,
1128,
3575,
5785,
5055,
8228,
1914,
6012,
void,
5171,
3810,
3053,
1775,
2437,
1751,
5214,
6904,
3496,
5429,
2340,
5683,
3428,
void,
27584,
19252,
void,
23558,
8432,
void,
8727,
6023,
1708,
1839,
6467,
2494,
6788,
6905,
2160,
2380,
void,
1023,
2142,
10754,
7460,
5136,
void,
4181,
1605,
2243,
9554,
4041,
4423,
void,
38394,
void,
5137,
2560,
1230,
7342,
5300,
3089,
1281,
7567,
1529,
3628,
void,
4026,
8121,
4227,
14794,
7567,
void,
4344,
12998,
5424,
2987,
10128,
8362,
void,
9373,
5522,
2017,
10260,
5263,
3402,
7981,
void,
6772,
2092,
6024,
4548,
1229,
2486,
4733,
3112,
4908,
3598,
5839,
2922,
4936,
void,
7723,
2389,
2100,
4448,
7255,
3430,
2808,
4832,
5848,
7024,
void,
4664,
4953,
4403,
3171,
6125,
4154,
2921,
3502,
7290,
6371,
2314,
1391,
void,
4967,
5243,
6186,
3218,
2082,
4625,
5827,
2257,
2186,
5716,
1032,
1796,
2609,
5704,
void,
66296,
void,
6973,
4755,
6328,
6720,
8118,
2696,
4449,
3474,
5110,
5852,
void,
8320,
10410,
7618,
6504,
1892,
9355,
2792,
6286,
void,
47376,
void,
1448,
1176,
6150,
3289,
1759,
5919,
2579,
5630,
3798,
6203,
4909,
5294,
1471,
4537,
void,
7656,
12471,
14457,
11694,
8007,
void,
3990,
8373,
1412,
1462,
5601,
8555,
8500,
2545,
8920,
void,
1312,
6198,
2638,
1095,
5201,
4953,
3056,
2526,
3699,
5639,
4583,
4737,
2245,
void,
7833,
5242,
6384,
7309,
3805,
6525,
6070,
6074,
5899,
2583,
void,
65699,
void,
6154,
3691,
1858,
5141,
6915,
2134,
2131,
5295,
4071,
4552,
1753,
6950,
3909,
void,
2587,
1337,
3790,
3137,
6150,
1698,
3950,
4046,
2308,
3871,
1824,
1533,
1930,
3360,
void,
10305,
13340,
6892,
void,
4945,
6137,
10521,
2418,
9791,
7035,
4151,
5497,
void,
25807,
25417,
void,
5771,
6122,
4596,
4022,
9682,
4696,
8749,
void,
1028,
6870,
3606,
4626,
4557,
6908,
5503,
1803,
2561,
2487,
3368,
5319,
void,
2027,
4102,
5920,
3335,
5804,
3829,
5977,
1592,
2113,
3771,
3938,
3417,
2550,
1980,
void,
3321,
4002,
1229,
3591,
3851,
1384,
2618,
4049,
3336,
2035,
1231,
2126,
1901,
2834,
5604,
void,
45987,
void,
3667,
3442,
2721,
5515,
9294,
4897,
7333,
2518,
4009,
void,
5686,
2042,
1130,
5463,
2814,
2780,
1037,
5490,
3188,
2882,
3742,
4048,
3203,
1785,
void,
20148,
14705,
void,
6293,
1673,
6865,
2414,
6552,
3886,
5944,
6538,
2569,
3770,
7698,
void,
1588,
2025,
2071,
4336,
4768,
1826,
2939,
3016,
1529,
3092,
1326,
5698,
5864,
4837,
4188,
void,
9974,
3626,
6637,
6988,
7509,
9460,
7542,
7164,
void,
5343,
1913,
12824,
13105,
13538,
1114,
void,
4699,
4692,
4170,
6325,
1634,
4627,
2721,
5309,
3772,
2132,
1894,
5094,
6416,
2492,
void,
22223,
15163,
3520,
void,
9437,
25803,
void,
2519,
7012,
2571,
6251,
1380,
5344,
6362,
2465,
4457,
1071,
5560,
1992,
void,
5522,
3334,
2597,
2382,
4118,
1624,
6175,
4656,
6525,
6593,
4459,
1349,
1403,
void,
1443,
7693,
10526,
9927,
9729,
3172,
7632,
void,
3154,
6472,
3815,
3055,
1009,
8458,
3250,
1864,
6734,
8458,
void,
3155,
2476,
6384,
2045,
2773,
7447,
7646,
1186,
5360,
4499,
2502,
void,
9675,
8902,
12054,
9405,
8558,
8901,
void,
3605,
2465,
9906,
5145,
13502,
9839,
void,
9207,
7212,
4778,
9929,
6582,
9680,
3349,
1813,
void,
4483,
15153,
22152,
void,
24781,
33010,
void,
19696,
void,
6451,
5583,
3639,
3565,
7956,
4005,
5236,
3053,
2192,
5485,
3143,
void,
3990,
1296,
4262,
4493,
4072,
5136,
4507,
4191,
6636,
5514,
5759,
6471,
3567,
void,
2296,
5998,
6269,
3935,
1929,
3188,
5768,
2547,
1318,
3689,
1161,
3894,
1206,
1304,
void,
2942,
2195,
5594,
8418,
3600,
6710,
4414,
7753,
1672,
1878,
void,
6682,
9976,
18511,
17026,
void,
7606,
3587,
2427,
9258,
8752,
7387,
4096,
3911,
6200,
void,
4364,
8277,
5476,
1718,
3530,
6895,
4822,
3970,
3368,
void,
12691,
3502,
4641,
8562,
8062,
5216,
void,
7316,
7688,
5102,
7950,
4291,
9711,
3202,
7487,
void,
57056,
void,
4268,
1730,
3617,
5257,
5760,
3222,
3497,
4891,
1584,
2587,
5954,
3391,
1203,
2673,
void,
15671,
11805,
1242,
void,
5560,
1774,
2332,
2068,
6859,
3012,
6677,
6255,
1178,
3695,
5131,
2075,
1398,
void,
8310,
2657,
1795,
14376,
void,
4270,
1931,
6894,
5011,
3766,
1028,
1405,
3768,
5249,
3972,
5799,
4704,
void,
4331,
3682,
1748,
6674,
4191,
4711,
4971,
3329,
5968,
1971,
1533,
5559,
void,
4569,
5005,
4292,
3541,
4578,
5532,
4163,
2555,
1936,
5767,
1299,
5620,
4897,
5824,
void,
2350,
3735,
7113,
6094,
7952,
5611,
3634,
8849,
5171,
void,
7046,
11580,
1720,
1574,
12103,
5747,
5966,
void,
10027,
6090,
9627,
4738,
9248,
5177,
3622,
5265,
void,
4617,
8158,
2629,
4317,
8707,
1442,
1944,
3006,
7283,
4939,
void,
2744,
3616,
1211,
4479,
5857,
6062,
5475,
1369,
7243,
4930,
5514,
3919,
void,
5938,
2249,
18108,
6713,
void,
6017,
1815,
2551,
2246,
5082,
3795,
3673,
1083,
5008,
6903,
6492,
1272,
1494,
void,
5350,
4727,
5366,
1227,
6846,
1355,
5226,
7069,
3365,
4011,
2857,
1696,
void,
2732,
3684,
5801,
3059,
1099,
5909,
4528,
2645,
2363,
1622,
3335,
2903,
4690,
1443,
void,
2445,
4940,
4878,
5229,
6686,
5416,
8016,
5249,
void,
1875,
2976,
3545,
2571,
2691,
5763,
4373,
5126,
5078,
2895,
3976,
1934,
4212,
void,
1649,
2120,
2801,
1222,
4478,
7415,
7956,
2507,
3464,
1512,
3680,
void,
3087,
5058,
5726,
4050,
5667,
1709,
4970,
1805,
4369,
3211,
2953,
1847,
5848,
3365,
5717,
void,
18473,
void,
2897,
15970,
13899,
void,
2396,
2432,
4315,
1646,
2933,
3701,
3034,
6228,
4978,
1254,
5964,
4797,
3712,
4613,
void,
3583,
4652,
2589,
3126,
1356,
3268,
4372,
5851,
3159,
2936,
2568,
1631,
4557,
4254,
5537,
void,
1796,
1304,
1129,
3237,
7542,
4587,
3158,
3055,
5129,
5981,
3396,
void,
2326,
2916,
7617,
1438,
1205,
4903,
7842,
6171,
6042,
6076,
8009,
void,
60341,
void,
10527,
30521,
void,
1882,
7475,
4092,
10469,
11258,
2361,
6750,
void,
7307,
10450,
2273,
3998,
4324,
4453,
5606,
void,
14605,
8618,
19203,
6177,
void,
40070,
void,
13758,
9814,
16312,
14985,
8629,
void,
20716,
void,
2344,
9535,
6583,
10900,
12041,
3724,
void,
3075,
11273,
9002,
9802,
1148,
9197,
10064,
void,
25216,
11228,
15709,
void,
21181,
10820,
8812,
void,
3073,
5764,
8201,
4269,
3302,
1795,
5387,
2618,
7288,
8388,
void,
24566,
void,
7608,
1738,
3212,
7028,
6321,
1087,
3361,
1698,
5582,
4314,
void,
9189,
9689,
7007,
2845,
1086,
9086,
1795,
6550,
7610,
void,
1139,
4157,
4135,
9875,
7720,
8262,
2666,
7147]).
