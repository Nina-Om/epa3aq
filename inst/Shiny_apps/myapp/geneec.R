geneec <- function(Input) {
  
  warn=geneec_err(Input)
  if (!warn$"Errors:" %in% c("No errors! All input parameters passed.")) {
    stop()
  }

AGASAE = function(NASAE,YLOC,PONDEP){

  X0 =c(
    24.1087, 23.0052, 22.0199, 21.1251, 20.3037, 19.5370, 18.8120,
    18.1255, 17.4778, 16.8699, 16.3036, 15.7771, 15.2845, 14.8218,
    14.3861, 13.9741, 13.5811, 13.2046, 12.8461, 12.5081, 12.1911,
    11.8934, 11.6117, 11.3433, 11.0870, 10.8427, 10.6099, 10.3879,
    10.1774,  9.9792,  9.7921,  9.6141,  9.4434,  9.2784,  9.1195,
    8.9671,  8.8211,  8.6811,  8.5475,  8.4198,  8.2968,  8.1778,
    8.0622,  7.9497,  7.8408,  7.7360,  7.6354,  7.5393,  7.4472,
    7.3584,  7.2722,  7.1882,  7.1064,  7.0271,  6.9505,  6.8766,
    6.8052,  6.7363,  6.6692,  6.6036,  6.5391,  6.4756,  6.4134,
    6.3528,  6.2937,  6.2363,  6.1806,  6.1261,  6.0727,  6.0200,
    5.9676,  5.9159,  5.8653,  5.8161,  5.7684,  5.7222,  5.6769,
    5.6324,  5.5883,  5.5439,  5.4993,  5.4553,  5.4124,  5.3711,
    5.3312,  5.2920,  5.2534,  5.2154,  5.1778,  5.1404,  5.1033,
    5.0671,  5.0321,  4.9982,  4.9647,  4.9312,  4.8983,  4.8660,
    4.8340,  4.8022,  4.7706,  4.7400,  4.7102,  4.6807,  4.6513,
    4.6223,  4.5938,  4.5656,  4.5374,  4.5094,  4.4821,  4.4559,
    4.4301,  4.4044,  4.3790,  4.3537,  4.3286,  4.3036,  4.2789,
    4.2547,  4.2309,  4.2073,  4.1840,  4.1609,  4.1381,  4.1157,
    4.0936,  4.0718,  4.0502,  4.0288,  4.0076,  3.9866,  3.9659,
    3.9455,  3.9254,  3.9055,  3.8860,  3.8666,  3.8473,  3.8283,
    3.8094,  3.7908,  3.7724,  3.7543,  3.7364,  3.7188,  3.7013,
    3.6839,  3.6667,  3.6496,  3.6327,  3.6160,  3.5996)

  X1 =c(
    12.9565, 11.6976, 10.7250,  9.9284,  9.2564,  8.6758,  8.1579,
    7.6828,  7.2376,  6.8193,  6.4343,  6.0883,  5.7788,  5.4992,
    5.2422,  5.0067,  4.7928,  4.5965,  4.4152,  4.2464,  4.0885,
    3.9416,  3.8053,  3.6786,  3.5614,  3.4534,  3.3534,  3.2602,
    3.1730,  3.0908,  3.0125,  2.9383,  2.8686,  2.8025,  2.7395,
    2.6791,  2.6214,  2.5665,  2.5141,  2.4641,  2.4163,  2.3706,
    2.3267,  2.2846,  2.2443,  2.2056,  2.1688,  2.1336,  2.1000,
    2.0678,  2.0370,  2.0074,  1.9790,  1.9517,  1.9255,  1.9003,
    1.8760,  1.8527,  1.8301,  1.8084,  1.7874,  1.7670,  1.7473,
    1.7282,  1.7097,  1.6918,  1.6744,  1.6576,  1.6412,  1.6252,
    1.6097,  1.5945,  1.5797,  1.5653,  1.5513,  1.5375,  1.5241,
    1.5109,  1.4980,  1.4853,  1.4729,  1.4607,  1.4487,  1.4370,
    1.4255,  1.4141,  1.4030,  1.3920,  1.3812,  1.3706,  1.3602,
    1.3500,  1.3399,  1.3300,  1.3203,  1.3107,  1.3012,  1.2919,
    1.2827,  1.2737,  1.2648,  1.2560,  1.2474,  1.2390,  1.2306,
    1.2223,  1.2142,  1.2062,  1.1983,  1.1906,  1.1830,  1.1756,
    1.1682,  1.1609,  1.1537,  1.1466,  1.1396,  1.1328,  1.1260,
    1.1193,  1.1127,  1.1061,  1.0996,  1.0932,  1.0868,  1.0805,
    1.0742,  1.0681,  1.0620,  1.0560,  1.0501,  1.0443,  1.0385,
    1.0327,  1.0270,  1.0214,  1.0158,  1.0103,  1.0049,  0.9996,
    0.9943,  0.9890,  0.9838,  0.9786,  0.9734,  0.9684,  0.9633,
    0.9584,  0.9534,  0.9485,  0.9436,  0.9388,  0.9339)

  X2 =c(
    9.1624,  7.8767,  6.9513,  6.2564,  5.7050,  5.2531,  4.8692,
    4.5300,  4.2203,  3.9311,  3.6628,  3.4225,  3.2145,  3.0354,
    2.8769,  2.7324,  2.5989,  2.4759,  2.3632,  2.2597,  2.1654,
    2.0800,  2.0018,  1.9286,  1.8593,  1.7936,  1.7315,  1.6731,
    1.6187,  1.5682,  1.5210,  1.4767,  1.4349,  1.3952,  1.3576,
    1.3219,  1.2883,  1.2567,  1.2267,  1.1983,  1.1713,  1.1457,
    1.1213,  1.0981,  1.0760,  1.0551,  1.0351,  1.0160,  0.9978,
    0.9803,  0.9635,  0.9473,  0.9318,  0.9168,  0.9025,  0.8887,
    0.8754,  0.8626,  0.8503,  0.8384,  0.8269,  0.8158,  0.8050,
    0.7946,  0.7846,  0.7749,  0.7656,  0.7565,  0.7477,  0.7391,
    0.7308,  0.7226,  0.7147,  0.7071,  0.6996,  0.6923,  0.6852,
    0.6782,  0.6714,  0.6646,  0.6580,  0.6516,  0.6452,  0.6390,
    0.6330,  0.6270,  0.6212,  0.6154,  0.6097,  0.6041,  0.5986,
    0.5932,  0.5880,  0.5828,  0.5777,  0.5727,  0.5677,  0.5628,
    0.5580,  0.5533,  0.5486,  0.5440,  0.5396,  0.5352,  0.5308,
    0.5265,  0.5223,  0.5181,  0.5140,  0.5100,  0.5061,  0.5023,
    0.4985,  0.4948,  0.4911,  0.4875,  0.4839,  0.4804,  0.4770,
    0.4736,  0.4703,  0.4670,  0.4637,  0.4605,  0.4573,  0.4541,
    0.4510,  0.4480,  0.4449,  0.4420,  0.4390,  0.4361,  0.4333,
    0.4304,  0.4276,  0.4249,  0.4221,  0.4195,  0.4169,  0.4143,
    0.4117,  0.4091,  0.4066,  0.4041,  0.4017,  0.3992,  0.3968,
    0.3945,  0.3922,  0.3899,  0.3876,  0.3853,  0.3830)

  X3 =c(
    7.1216,  5.8417,  4.9591,  4.3198,  3.8360,  3.4540,  3.1474,
    2.8969,  2.6828,  2.4911,  2.3152,  2.1551,  2.0131,  1.8902,
    1.7827,  1.6860,  1.5973,  1.5154,  1.4404,  1.3717,  1.3089,
    1.2515,  1.1987,  1.1497,  1.1041,  1.0619,  1.0226,  0.9860,
    0.9521,  0.9206,  0.8913,  0.8642,  0.8390,  0.8156,  0.7937,
    0.7733,  0.7542,  0.7362,  0.7194,  0.7035,  0.6886,  0.6745,
    0.6612,  0.6486,  0.6367,  0.6254,  0.6147,  0.6045,  0.5947,
    0.5854,  0.5765,  0.5679,  0.5596,  0.5517,  0.5440,  0.5367,
    0.5295,  0.5226,  0.5159,  0.5094,  0.5032,  0.4971,  0.4912,
    0.4855,  0.4799,  0.4744,  0.4690,  0.4638,  0.4587,  0.4537,
    0.4488,  0.4440,  0.4393,  0.4347,  0.4302,  0.4257,  0.4213,
    0.4170,  0.4128,  0.4086,  0.4045,  0.4005,  0.3965,  0.3926,
    0.3888,  0.3850,  0.3813,  0.3776,  0.3740,  0.3705,  0.3670,
    0.3635,  0.3601,  0.3568,  0.3535,  0.3502,  0.3470,  0.3439,
    0.3408,  0.3377,  0.3347,  0.3318,  0.3289,  0.3260,  0.3232,
    0.3205,  0.3177,  0.3151,  0.3125,  0.3099,  0.3074,  0.3049,
    0.3025,  0.3001,  0.2978,  0.2955,  0.2932,  0.2910,  0.2888,
    0.2867,  0.2846,  0.2825,  0.2805,  0.2784,  0.2764,  0.2745,
    0.2725,  0.2706,  0.2688,  0.2669,  0.2651,  0.2633,  0.2615,
    0.2598,  0.2580,  0.2563,  0.2546,  0.2530,  0.2514,  0.2498,
    0.2482,  0.2466,  0.2451,  0.2435,  0.2420,  0.2405,  0.2390,
    0.2376,  0.2361,  0.2347,  0.2333,  0.2319,  0.2305)

  X4 =c(
    2.8606,  1.5995,  1.2498,  1.0650,  0.9471,  0.8619,  0.7959,
    0.7423,  0.6973,  0.6588,  0.6251,  0.5954,  0.5687,  0.5447,
    0.5229,  0.5029,  0.4845,  0.4676,  0.4518,  0.4371,  0.4234,
    0.4106,  0.3985,  0.3872,  0.3764,  0.3663,  0.3567,  0.3475,
    0.3389,  0.3306,  0.3227,  0.3152,  0.3080,  0.3011,  0.2945,
    0.2882,  0.2821,  0.2763,  0.2706,  0.2652,  0.2600,  0.2550,
    0.2502,  0.2455,  0.2410,  0.2366,  0.2324,  0.2283,  0.2244,
    0.2205,  0.2168,  0.2132,  0.2097,  0.2063,  0.2030,  0.1998,
    0.1967,  0.1937,  0.1907,  0.1878,  0.1850,  0.1823,  0.1797,
    0.1771,  0.1746,  0.1721,  0.1697,  0.1674,  0.1651,  0.1629,
    0.1607,  0.1586,  0.1565,  0.1545,  0.1525,  0.1506,  0.1487,
    0.1468,  0.1450,  0.1432,  0.1415,  0.1398,  0.1381,  0.1365,
    0.1349,  0.1333,  0.1318,  0.1303,  0.1288,  0.1274,  0.1260,
    0.1246,  0.1232,  0.1219,  0.1205,  0.1192,  0.1180,  0.1167,
    0.1155,  0.1143,  0.1131,  0.1120,  0.1108,  0.1097,  0.1086,
    0.1075,  0.1065,  0.1054,  0.1044,  0.1034,  0.1024,  0.1014,
    0.1004,  0.0995,  0.0986,  0.0976,  0.0967,  0.0958,  0.0950,
    0.0941,  0.0933,  0.0924,  0.0916,  0.0908,  0.0900,  0.0892,
    0.0884,  0.0877,  0.0869,  0.0862,  0.0854,  0.0847,  0.0840,
    0.0833,  0.0826,  0.0819,  0.0812,  0.0806,  0.0799,  0.0793,
    0.0786,  0.0780,  0.0774,  0.0768,  0.0762,  0.0756,  0.0750,
    0.0744,  0.0738,  0.0733,  0.0727,  0.0722,  0.0716)

  X5 =c(
    6.5841,  4.5102,  3.5507,  2.9838,  2.6109,  2.3424,  2.1356,
    1.9687,  1.8297,  1.7112,  1.6084,  1.5180,  1.4375,  1.3653,
    1.3000,  1.2406,  1.1862,  1.1362,  1.0900,  1.0472,  1.0073,
    0.9702,  0.9354,  0.9028,  0.8722,  0.8433,  0.8161,  0.7903,
    0.7659,  0.7428,  0.7208,  0.6999,  0.6800,  0.6611,  0.6430,
    0.6257,  0.6092,  0.5933,  0.5782,  0.5636,  0.5497,  0.5363,
    0.5234,  0.5110,  0.4990,  0.4876,  0.4765,  0.4658,  0.4555,
    0.4456,  0.4360,  0.4267,  0.4177,  0.4090,  0.4006,  0.3925,
    0.3846,  0.3770,  0.3696,  0.3624,  0.3554,  0.3487,  0.3421,
    0.3358,  0.3296,  0.3235,  0.3177,  0.3120,  0.3065,  0.3011,
    0.2959,  0.2908,  0.2858,  0.2810,  0.2763,  0.2717,  0.2672,
    0.2628,  0.2586,  0.2544,  0.2504,  0.2464,  0.2425,  0.2388,
    0.2351,  0.2315,  0.2280,  0.2245,  0.2212,  0.2179,  0.2147,
    0.2116,  0.2085,  0.2055,  0.2026,  0.1997,  0.1969,  0.1941,
    0.1915,  0.1888,  0.1862,  0.1837,  0.1812,  0.1788,  0.1764,
    0.1741,  0.1718,  0.1696,  0.1674,  0.1652,  0.1631,  0.1610,
    0.1590,  0.1570,  0.1551,  0.1532,  0.1513,  0.1494,  0.1476,
    0.1458,  0.1441,  0.1424,  0.1407,  0.1390,  0.1374,  0.1358,
    0.1342,  0.1327,  0.1312,  0.1297,  0.1282,  0.1268,  0.1254,
    0.1240,  0.1226,  0.1213,  0.1200,  0.1187,  0.1174,  0.1161,
    0.1149,  0.1137,  0.1125,  0.1113,  0.1101,  0.1090,  0.1079,
    0.1068,  0.1057,  0.1046,  0.1036,  0.1025,  0.1015)

  X6 =c(
    0.7554,  0.5749,  0.5271,  0.4925,  0.4646,  0.4409,  0.4202,
    0.4020,  0.3856,  0.3709,  0.3575,  0.3451,  0.3338,  0.3232,
    0.3134,  0.3043,  0.2957,  0.2876,  0.2800,  0.2728,  0.2660,
    0.2596,  0.2535,  0.2476,  0.2421,  0.2368,  0.2317,  0.2269,
    0.2222,  0.2177,  0.2134,  0.2093,  0.2054,  0.2015,  0.1978,
    0.1943,  0.1909,  0.1876,  0.1843,  0.1812,  0.1782,  0.1753,
    0.1725,  0.1698,  0.1671,  0.1646,  0.1621,  0.1597,  0.1573,
    0.1550,  0.1528,  0.1506,  0.1485,  0.1464,  0.1444,  0.1425,
    0.1406,  0.1387,  0.1369,  0.1351,  0.1334,  0.1317,  0.1300,
    0.1284,  0.1268,  0.1253,  0.1238,  0.1223,  0.1209,  0.1195,
    0.1181,  0.1167,  0.1154,  0.1141,  0.1128,  0.1116,  0.1104,
    0.1092,  0.1080,  0.1069,  0.1057,  0.1046,  0.1035,  0.1025,
    0.1014,  0.1004,  0.0994,  0.0984,  0.0974,  0.0965,  0.0955,
    0.0946,  0.0937,  0.0928,  0.0919,  0.0910,  0.0902,  0.0894,
    0.0885,  0.0877,  0.0869,  0.0862,  0.0854,  0.0846,  0.0839,
    0.0832,  0.0824,  0.0817,  0.0810,  0.0803,  0.0796,  0.0790,
    0.0783,  0.0777,  0.0770,  0.0764,  0.0758,  0.0752,  0.0745,
    0.0740,  0.0734,  0.0728,  0.0722,  0.0716,  0.0711,  0.0705,
    0.0700,  0.0695,  0.0689,  0.0684,  0.0679,  0.0674,  0.0669,
    0.0664,  0.0659,  0.0654,  0.0650,  0.0645,  0.0640,  0.0636,
    0.0631,  0.0627,  0.0622,  0.0618,  0.0614,  0.0609,  0.0605,
    0.0601,  0.0597,  0.0593,  0.0589,  0.0585,  0.0581)

  X7 =c(
    1.2189,  0.9356,  0.8528,  0.7931,  0.7449,  0.7040,  0.6685,
    0.6372,  0.6092,  0.5840,  0.5610,  0.5400,  0.5207,  0.5028,
    0.4862,  0.4707,  0.4562,  0.4426,  0.4297,  0.4177,  0.4062,
    0.3954,  0.3852,  0.3754,  0.3661,  0.3573,  0.3488,  0.3407,
    0.3330,  0.3256,  0.3185,  0.3117,  0.3051,  0.2988,  0.2927,
    0.2869,  0.2812,  0.2758,  0.2705,  0.2654,  0.2605,  0.2558,
    0.2512,  0.2467,  0.2424,  0.2383,  0.2342,  0.2303,  0.2264,
    0.2227,  0.2191,  0.2156,  0.2122,  0.2089,  0.2057,  0.2025,
    0.1995,  0.1965,  0.1936,  0.1907,  0.1880,  0.1853,  0.1827,
    0.1801,  0.1776,  0.1751,  0.1728,  0.1704,  0.1681,  0.1659,
    0.1637,  0.1616,  0.1595,  0.1575,  0.1555,  0.1535,  0.1516,
    0.1497,  0.1479,  0.1461,  0.1443,  0.1426,  0.1409,  0.1393,
    0.1376,  0.1360,  0.1345,  0.1329,  0.1314,  0.1300,  0.1285,
    0.1271,  0.1257,  0.1243,  0.1230,  0.1216,  0.1203,  0.1191,
    0.1178,  0.1166,  0.1154,  0.1142,  0.1130,  0.1118,  0.1107,
    0.1096,  0.1085,  0.1074,  0.1064,  0.1053,  0.1043,  0.1033,
    0.1023,  0.1013,  0.1003,  0.0994,  0.0985,  0.0975,  0.0966,
    0.0957,  0.0949,  0.0940,  0.0931,  0.0923,  0.0915,  0.0907,
    0.0899,  0.0891,  0.0883,  0.0875,  0.0868,  0.0860,  0.0853,
    0.0845,  0.0838,  0.0831,  0.0824,  0.0817,  0.0811,  0.0804,
    0.0797,  0.0791,  0.0784,  0.0778,  0.0772,  0.0766,  0.0759,
    0.0753,  0.0748,  0.0742,  0.0736,  0.0730,  0.0724)

  X8 =c(
    0.5141,  0.3426,  0.2649,  0.2185,  0.1875,  0.1649,  0.1474,
    0.1334,  0.1218,  0.1120,  0.1036,  0.0963,  0.0898,  0.0841,
    0.0790,  0.0745,  0.0703,  0.0665,  0.0631,  0.0599,  0.0570,
    0.0544,  0.0519,  0.0496,  0.0474,  0.0454,  0.0436,  0.0418,
    0.0402,  0.0386,  0.0372,  0.0358,  0.0345,  0.0333,  0.0321,
    0.0311,  0.0300,  0.0290,  0.0281,  0.0272,  0.0264,  0.0256,
    0.0248,  0.0241,  0.0233,  0.0227,  0.0220,  0.0214,  0.0208,
    0.0203,  0.0197,  0.0192,  0.0187,  0.0182,  0.0178,  0.0173,
    0.0169,  0.0165,  0.0161,  0.0157,  0.0153,  0.0150,  0.0146,
    0.0143,  0.0140,  0.0137,  0.0134,  0.0131,  0.0128,  0.0125,
    0.0122,  0.0120,  0.0117,  0.0115,  0.0113,  0.0110,  0.0108,
    0.0106,  0.0104,  0.0102,  0.0100,  0.0098,  0.0096,  0.0095,
    0.0093,  0.0091,  0.0090,  0.0088,  0.0086,  0.0085,  0.0083,
    0.0082,  0.0080,  0.0079,  0.0078,  0.0076,  0.0075,  0.0074,
    0.0073,  0.0072,  0.0070,  0.0069,  0.0068,  0.0067,  0.0066,
    0.0065,  0.0064,  0.0063,  0.0062,  0.0061,  0.0060,  0.0059,
    0.0059,  0.0058,  0.0057,  0.0056,  0.0055,  0.0054,  0.0054,
    0.0053,  0.0052,  0.0051,  0.0051,  0.0050,  0.0049,  0.0049,
    0.0048,  0.0047,  0.0047,  0.0046,  0.0046,  0.0045,  0.0044,
    0.0044,  0.0043,  0.0043,  0.0042,  0.0042,  0.0041,  0.0041,
    0.0040,  0.0040,  0.0039,  0.0039,  0.0038,  0.0038,  0.0037,
    0.0037,  0.0036,  0.0036,  0.0036,  0.0035,  0.0035)

  X9 =c(
    3.2169,  2.6677,  2.2877,  2.0032,  1.7817,  1.6032,  1.4557,
    1.3314,  1.2250,  1.1330,  1.0524,  0.9813,  0.9180,  0.8614,
    0.8105,  0.7644,  0.7225,  0.6842,  0.6492,  0.6170,  0.5874,
    0.5599,  0.5345,  0.5109,  0.4889,  0.4684,  0.4493,  0.4313,
    0.4144,  0.3986,  0.3837,  0.3697,  0.3564,  0.3439,  0.3320,
    0.3208,  0.3102,  0.3000,  0.2904,  0.2813,  0.2726,  0.2643,
    0.2564,  0.2488,  0.2416,  0.2347,  0.2281,  0.2218,  0.2157,
    0.2099,  0.2044,  0.1990,  0.1939,  0.1889,  0.1842,  0.1796,
    0.1752,  0.1710,  0.1669,  0.1629,  0.1591,  0.1555,  0.1519,
    0.1485,  0.1452,  0.1420,  0.1389,  0.1359,  0.1330,  0.1302,
    0.1275,  0.1249,  0.1223,  0.1199,  0.1175,  0.1152,  0.1129,
    0.1107,  0.1086,  0.1065,  0.1045,  0.1026,  0.1007,  0.0988,
    0.0970,  0.0953,  0.0936,  0.0919,  0.0903,  0.0887,  0.0872,
    0.0857,  0.0843,  0.0829,  0.0815,  0.0801,  0.0788,  0.0775,
    0.0763,  0.0751,  0.0739,  0.0727,  0.0716,  0.0705,  0.0694,
    0.0683,  0.0673,  0.0663,  0.0653,  0.0643,  0.0634,  0.0624,
    0.0615,  0.0606,  0.0598,  0.0589,  0.0581,  0.0573,  0.0565,
    0.0557,  0.0549,  0.0542,  0.0535,  0.0527,  0.0520,  0.0513,
    0.0507,  0.0500,  0.0494,  0.0487,  0.0481,  0.0475,  0.0469,
    0.0463,  0.0457,  0.0451,  0.0446,  0.0440,  0.0435,  0.0430,
    0.0424,  0.0419,  0.0414,  0.0409,  0.0404,  0.0400,  0.0395,
    0.0390,  0.0386,  0.0381,  0.0377,  0.0373,  0.0369)

  XV = cbind(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9)

  N=max(0,min(9,as.integer(NASAE)))
  I=max(1,min(152,as.integer(as.integer(0.5*YLOC+1))))
  YM=2.0*(I-1)
  YP=2.0*I
  PONDEP=0.5*(XV[[I,N+1]]*(YP-YLOC)+XV[[I+1,N+1]]*(YLOC-YM))
  PONDEP
}

CONC0_df <- c()
CONC4_df <- c()
CONC21_df <- c()
CONC60_df <- c()
CONC90_df <- c()
res_df <- c()
CODE_df <- c()
CROP_df <- c()
CHMNAM_df <- c() 

for (l in 2:dim(Input)[2]){
  ADSFRR <- c()
  DEGFRF <- c()
  ROCONC <- c()
  ADSFRS <- c()
  SDCONC <- c()
  CHRONIC <- c()

  APPRAT = 0
  APPNUM = 0
  APPTOT = 0
  KD = 0
  KOC = 0
  KDFRAC = 0
  CORECT = 0
  SOL = 0
  METHAF = 0
  METHAP = 0
  HYDHAP = 0
  FOTHAP = 0
  DEGHAP = 0
  INCORP = 0
  PCTSRO = 0
  ROAREA = 0
  ROINIT = 0
  ROFIN = 0
  SDINIT = 0
  SDFIN = 0
  SHIFT = 0
  STORM = 0
  KADS1 = 0
  KADSUR = 0
  KADSUS = 0
  KDEGP = 0
  KMETF = 0
  KHYDP = 0
  KMETP = 0
  KFOTP = 0
  CONC0 = 0
  CONC4 = 0
  CONC21 = 0
  CONC60 = 0
  CONC90 = 0
  SUM4 = 0
  SUM21 = 0
  SUM60 = 0
  SUM90 = 0

  for(K in 1:99){
    ROCONC[K] = 0
    SDCONC[K] = 0
    CHRONIC[K] = 0
  }
  for(K in 1:100){
    ADSFRR[K] = 0
    ADSFRS[K] = 0
  }

  for(K in 1:8){
    DEGFRF[K] = 0
  }
  I = 0
  K = 0
##################################################
CODE = as.numeric(l-1)
CHMNAM = Input[1,l]
CROP=Input[2,l]
APPRAT = as.numeric(as.character(Input[3,l]))

# if(APPRAT <= 0.0){
#   METRAT = as.numeric(as.character(Input[4,l]))
#   APPRAT = METRAT * 0.890309
# }

APPNUM = as.numeric(as.character(Input[4,l]))
APPTOT = APPRAT * APPNUM

if (APPNUM > 1){
  APSPAC=as.numeric(as.character(Input[5,l]))
} else {
  APSPAC=1
}

TDEGF = APPNUM * APSPAC

KD=as.numeric(as.character(Input[6,l]))

if(KD <= 0.0){
  KOC=as.numeric(as.character(Input[7,l]))
  ADSORP = 'A'
  KD = 0.0116 * KOC
} else {
  KOC = KD * 86.207
  ADSORP = 'B'
}

STORM = 2
METHAF=as.numeric(as.character(Input[8,l]))
WETTED = Input[9,l]

if(WETTED == 'Y'| WETTED == 'y') STORM = 0
METHOD=Input[10,l]

if(METHOD =='A'| METHOD == 'a'){            #1
  AIRFLG=Input[11,l]

  if(AIRFLG == 'A' | AIRFLG =='a'){
    NASAE = 0
    SPTYPE = 'AERL_A'
    } else if (AIRFLG =='B'| AIRFLG =='b'){
      NASAE = 1
      SPTYPE = 'AERL_B'
      } else if (AIRFLG =='C'| AIRFLG =='c'){
        NASAE = 2
        SPTYPE = 'AERL_C'
        } else if (AIRFLG =='D'| AIRFLG =='d'){
          NASAE = 3
          SPTYPE = 'AERL_D'
          } else {
            print("PLEASE CHOOSE AN AERIAL DROPLET SIZE DISTRIBUTION (A, B, C, D)", sep="")
            #AIRFLG
          }

  YLOCEN=as.numeric(as.character(Input[15,l]))
  YLOC = YLOCEN * 0.3048
  PONDEP= AGASAE(NASAE,YLOC)
  DRIFT=(PONDEP/100.0)
  APPEFF=0.95

}else if (METHOD == 'B' | METHOD == 'b'){#1
    GRNFLG=Input[12,l]
    if(GRNFLG == 'A' | GRNFLG == 'a'){
      GRSIZE=Input[13,l]
      if(GRSIZE == 'A' | GRSIZE == 'a'){
        NASAE = 4
        SPTYPE = 'GRLOFI'
        }else if (GRSIZE == 'B' | GRSIZE == 'b'){
          NASAE = 6
          SPTYPE = 'GRLOME'
        }
    }else if (GRNFLG == 'B' | GRNFLG == 'b'){
        GRSIZE=Input[13,l]
        #Input[13]=GRSIZE
        if(GRSIZE == 'A' | GRSIZE == 'a'){
          NASAE = 5
          SPTYPE = 'GRHIFI'
          }else if (GRSIZE == 'B' | GRSIZE == 'b'){
            NASAE = 7
            SPTYPE = 'GRHIME'
            }
    }else{
          print("PLEASE ENTER THE NOZZLE HEIGHT ABOVE THE CROP OR GROUND(A or B)", sep="")
          }

YLOCEN=as.numeric(as.character(Input[15,l]))
YLOC = YLOCEN * 0.3048
PONDEP= AGASAE(NASAE,YLOC)
DRIFT=(PONDEP/100.0)
APPEFF=0.99

}else if(METHOD =='C' | METHOD =='c'){        #1
  ORCFLG=Input[14,l]
  if(ORCFLG =='A' | ORCFLG =='a'){
    NASAE = 9
    SPTYPE = 'ORCHAR'
  }else if(ORCFLG =='B' | ORCFLG =='b'){
      NASAE = 8
      SPTYPE = 'VINYAR'
  }else{
        print("PLEASE ENTER AIRBLAST TYPE (A or B)")
       }

  YLOCEN=as.numeric(as.character(Input[15,l]))
  YLOC = YLOCEN * 0.3048
  PONDEP= AGASAE(NASAE,YLOC)
  DRIFT=(PONDEP/100.0)
  DRIFT = DRIFT * 3.0
  APPEFF=0.99

}else if(METHOD == 'D' | METHOD == 'd'){
  YLOCEN = 0.0
  YLOC = 0.0
  DRIFT = 0.0
  APPEFF = 1.0
  SPTYPE = 'GRANUL'
}
#################################
if(METHOD == 'B' | METHOD == 'b'| METHOD == 'D' | METHOD == 'd'){
  INCORP=as.numeric(as.character(Input[16,l]))
  APFLAG = 0}
# }else{
#   INCORP=0.0001
#   APFLAG = 0
#   #Input[16]=1
#   }

SOL=as.numeric(as.character(Input[17,l]))

if(KD <= 5.00e-3){
KDFRAC = 1.0
}else if(KD <= 1.00e-2 & KD > 5.00e-3){
KDFRAC = 0.9991715 + (1.0 - 0.9991715) * (1.00e-2 - KD) /
(1.00e-2 - 5.00e-3)
}else if(KD <= 5.00e-2 & KD > 1.00e-2){
KDFRAC = 0.9933720 + (0.9991715 - 0.9933720) * (5.00e-2 - KD) /
(5.00e-2 - 1.00e-2)
}else if(KD <= 1.00e-1 & KD > 5.00e-2){
KDFRAC = 0.9859155 + (0.9933720 - 0.9859155) * (1.00e-1 - KD) /
(1.00e-1 - 5.00e-2)
}else if(KD <= 3.00e-1 & KD > 1.00e-1){
KDFRAC = 0.9569180 + (0.9859155 - 0.9569180) * (3.00e-1 - KD) /
(3.00e-1 - 1.00e-1)
}else if(KD <= 5.00e-1 & KD > 3.00e-1){
KDFRAC = 0.9295775 + (0.9569180 - 0.9295775) * (5.00e-1 - KD) /
(5.00e-1 - 3.00e-1)
}else if(KD <= 7.50e-1 & KD > 5.00e-1){
KDFRAC = 0.8980944 + (0.9295775 - 0.8980944) * (7.50e-1 - KD) /
(7.50e-1 - 5.00e-1)
}else if(KD <= 1.00e00 & KD > 7.50e-1){
KDFRAC = 0.8682684 + (0.8980944 - 0.8682684) * (1.00e00 - KD) /
(1.00e00 - 7.50e-1)
}else if(KD <= 1.25e00 & KD > 1.00e00){
KDFRAC = 0.8409279 + (0.8682684 - 0.8409279) * (1.25e00 - KD) /
(1.25e00 - 1.00e00)
}else if(KD <= 1.50e00 & KD > 1.25e00){
KDFRAC = 0.8147307 + (0.8409279 - 0.8147307) * (1.50e00 - KD) /
(1.50e00 - 1.25e00)
}else if(KD <= 1.75e00 & KD > 1.50e00){
KDFRAC = 0.7904060 + (0.8147307 - 0.7904060) * (1.75e00 - KD) /
(1.75e00 - 1.50e00)
}else if(KD <= 2.00e00 & KD > 1.75e00){
KDFRAC = 0.7675973 + (0.7904060 - 0.7675973) * (2.00e00 - KD) /
(2.00e00 - 1.75e00)
}else if(KD <= 2.20e00 & KD > 2.00e00){
KDFRAC = 0.7461475 + (0.7675973 - 0.7461475) * (2.25e00 - KD) /
(2.25e00 - 2.00e00)
}else if(KD <= 2.50e00 & KD > 2.25e00){
KDFRAC = 0.7260066 + (0.7461475 - 0.7260066) * (2.50e00 - KD) /
(2.50e00 - 2.25e00)
}else if(KD <= 2.75e00 & KD > 2.50e00){
KDFRAC = 0.7070340 + (0.7260066 - 0.7070340) * (2.75e00 - KD) /
(2.75e00 - 2.50e00)
}else if(KD <= 3.00e00 & KD > 2.75e00){
KDFRAC = 0.6891881 + (0.7070340 - 0.6891881) * (3.00e00 - KD) /
(3.00e00 - 2.75e00)
}else if(KD <= 3.50e00 & KD > 3.00e00){
KDFRAC = 0.6562883 + (0.6891881 - 0.6562883) * (3.50e00 - KD) /
(3.50e00 - 3.00e00)
}else if(KD <= 4.00e00 & KD > 3.50e00){
KDFRAC = 0.6269097 + (0.6562883 - 0.6269097) * (4.00e00 - KD) /
(4.00e00 - 3.50e00)
}else if(KD <= 4.50e00 & KD > 4.00e00){
KDFRAC = 0.6004060 + (0.6269097 - 0.6004060) * (4.50e00 - KD) /
(4.50e00 - 4.00e00)
}else if(KD <= 5.00e00 & KD > 4.50e00){
KDFRAC = 0.5765700 + (0.6004060 - 0.5765700) * (5.00e00 - KD) /
(5.00e00 - 4.50e00)
}else if(KD <= 5.50e00 & KD > 5.00e00){
KDFRAC = 0.5548384 + (0.5765700 - 0.5548384) * (5.50e00 - KD) /
(5.50e00 - 5.00e00)
}else if(KD <= 6.00e00 & KD > 5.50e00){
KDFRAC = 0.5352196 + (0.5548384 - 0.5352196) * (6.00e00 - KD) /
(6.00e00 - 5.50e00)
}else if(KD <= 7.00e00 & KD > 6.00e00){
KDFRAC = 0.5007954 + (0.5352196 - 0.5007954) * (7.00e00 - KD) /
(7.00e00 - 6.00e00)
}else if(KD <= 8.00e00 & KD > 7.00e00){
KDFRAC = 0.4717896 + (0.5007954 - 0.4717896) * (8.00e00 - KD) /
(8.00e00 - 7.00e00)
}else if(KD <= 9.00e00 & KD > 8.00e00){
KDFRAC = 0.4471002 + (0.4717896 - 0.4471002) * (9.00e00 - KD) /
(9.00e00 - 8.00e00)
}else if(KD <= 1.00e01 & KD > 9.00e00){
KDFRAC = 0.4257415 + (0.4471002 - 0.4257415) * (1.00e01 - KD) /
(1.00e01 - 9.00e00)
}else if(KD <= 1.25e01 & KD > 1.00e01){
KDFRAC = 0.3837614 + (0.4257415 - 0.3837614) * (1.25e01 - KD) /
(1.25e01 - 1.00e01)
}else if(KD <= 1.50e01 & KD > 1.25e01){
KDFRAC = 0.3528086 + (0.3837614 - 0.3528086) * (1.50e01 - KD) /
(1.50e01 - 1.25e01)
}else if(KD <= 1.75e01 & KD > 1.50e01){
KDFRAC = 0.3292129 + (0.3528086 - 0.3292129) * (1.75e01 - KD) /
(1.75e01 - 1.50e01)
}else if(KD <= 2.00e01 & KD > 1.75e01){
KDFRAC = 0.3107208 + (0.3292129 - 0.3107208) * (2.00e01 - KD) /
(2.00e01 - 1.75e01)
}else if(KD <= 2.50e01 & KD > 2.00e01){
KDFRAC = 0.2834880 + (0.3107208 - 0.2834880) * (2.50e01 - KD) /
(2.50e01 - 2.00e01)
}else if(KD <= 3.00e01 & KD > 2.50e01){
KDFRAC = 0.2646396 + (0.2834880 - 0.2646396) * (3.00e01 - KD) /
(3.00e01 - 2.50e01)
}else if(KD <= 4.00e01 & KD > 3.00e01){
KDFRAC = 0.2400580 + (0.2646396 - 0.2400580) * (4.00e01 - KD) /
(4.00e01 - 3.00e01)
}else if(KD <= 5.00e01 & KD > 4.00e01){
KDFRAC = 0.2249793 + (0.2400580 - 0.2249793) * (5.00e01 - KD) /
(5.00e01 - 4.00e01)
}else if(KD <= 1.00e02 & KD > 5.00e01){
KDFRAC = 0.1939188 + (0.2249793 - 0.1939188) * (1.00e02 - KD) /
(1.00e02 - 5.00e01)
}else if(KD <= 5.00e02 & KD > 1.00e02){
KDFRAC = 0.1788732 + (0.1939188 - 0.1788732) * (5.00e02 - KD) /
(5.00e02 - 1.00e02)
}else if(KD <= 1.00e03 & KD > 5.00e02){
KDFRAC = 0.1615742 + (0.1788732 - 0.1615742) * (1.00e03 - KD) /
(1.00e03 - 5.00e02)
}else if(KD <= 5.00e03 & KD > 1.00e03){
KDFRAC = 0.1425352 + (0.1615742 - 0.1425352) * (5.00e03 - KD) /
(5.00e03 - 1.00e03)
}else if(KD <= 1.00e04 & KD > 5.00e03){
KDFRAC = 0.1258409 + (0.1425352 - 0.1258409) * (1.00e04 - KD) /
(1.00e04 - 5.00e03)
}else if(KD <= 2.00e04 & KD > 1.00e04){
KDFRAC = 0.1021458 + (0.1258409 - 0.1021458) * (2.00e04 - KD) /
(2.00e04 - 1.00e04)
}else if(KD <= 3.00e04 & KD > 2.00e04){
KDFRAC = 0.0859983 + (0.1021458 - 0.0859983) * (3.00e04 - KD) /
(3.00e04 - 2.00e04)
}else if(KD <= 5.00e04 & KD > 3.00e04){
KDFRAC = 0.0653521 + (0.0859983 - 0.0653521) * (5.00e04 - KD) /
(5.00e04 - 3.00e04)
}else if(KD <= 1.00e05 & KD > 5.00e04){
KDFRAC = 0.0408318 + (0.0653521 - 0.0408318) * (1.00e05 - KD) /
(1.00e05 - 5.00e04)
}else if(KD <= 5.00e05 & KD > 1.00e05){
KDFRAC = 0.0102055 + (0.0408318 - 0.0102055) * (5.00e05 - KD) /
(5.00e05 - 1.00e05)
}else if(KD <= 1.00e06 & KD > 5.00e05){
KDFRAC = 0.0052672 + (0.0102055 - 0.0052672) * (1.00e06 - KD) /
(1.00e06 - 5.00e05)
}else if(KD > 1.00e06){
KDFRAC = 0.001
}

if (METHAF  <=  0.0){
  KMETF = 0.0
} else {
  KMETF = log(2.0) / METHAF
}

if(INCORP <= 0.0001) APFLAG = 1
if(INCORP <= 1.0) INCORP = 1.0
if(INCORP >= 6.0) INCORP = 6.0

PCTSRO = 0.10
ROAREA = 10.0
WBAREA = 1.0

SHIFT = 0

for(I in 1:8){
  SHIFT = I-1
  DEGFRF[I] = exp(-KMETF*SHIFT)
  DEGF1 = DEGFRF[1]
  DEGF2 = DEGFRF[2]
  DEGF3 = DEGFRF[3]
  DEGF4 = DEGFRF[4]
  DEGF5 = DEGFRF[5]
  DEGF6 = DEGFRF[6]
  DEGF7 = DEGFRF[7]
  DEGF8 = DEGFRF[8]
  DEGF9 = DEGFRF[9]
  DEGF10 = DEGFRF[10]
}
###################################
STORM = STORM + 1

METHAP=as.numeric(as.character(Input[18,l]))

if(METHAP <= 0.0) {
  KMETP = 0.0
  METHAP = 0.00
  HYDHAP=as.numeric(as.character(Input[19,l]))
  if(HYDHAP <= 0.0) {
    KHYDP = 0.0
  }else {
    KHYDP = log(2.0) / HYDHAP
  }
} else {
  HYDHAP= 0
  #added
  KMETP = log(2.0) / METHAP
  KHYDP = 0.0                  # added
}
##############################################################
FOTHAP=as.numeric(as.character(Input[20,l]))

if(FOTHAP <= 0.0) {
  KFOTP = 0.0
} else {
  KFOTP = (log(2.0) / FOTHAP) / 124
}

KDEGP = KHYDP + KFOTP + KMETP

if(KDEGP <= 0.0) {
  DEGHAP = 0.0
} else {
  DEGHAP = log(2.0) / KDEGP
}

KDEGP = 0.55 * KDEGP

PSTMSF <- c()
for(I in 1:600){
  PSTMSF[I] = 0.0
}

PSTMSP <- c()
for(I in 1:600){
  PSTMSP[I] = 0.0
}

I = 1
PSTMSF[1] = APPRAT

I = 1
PSTMSP[1] = APPRAT

if(METHAF <= 0.0) {
  KDEGF = 0.0
} else {
  KDEGF = log(2.0) / METHAF
}
###################################################
if(APPNUM == 1){
  PSTMSF[I] = APPRAT
  CHECK1 = PSTMSF[1]
  CHECK2 = PSTMSF[2]
  CHECK3 = PSTMSF[3]
  CHECK4 = PSTMSF[4]
  CHECK5 = PSTMSF[5]
  CHECK6 = PSTMSF[6]
  CHECK7 = PSTMSF[7]
  }else{
    for(I in 2:APPNUM){
      PSTMSF[I] = PSTMSF[I-1] * exp(-KDEGF*APSPAC) + APPRAT
      CHECK1 = PSTMSF[1]
      CHECK2 = PSTMSF[2]
      CHECK3 = PSTMSF[3]
      CHECK4 = PSTMSF[4]
      CHECK5 = PSTMSF[5]
      CHECK6 = PSTMSF[6]
      CHECK7 = PSTMSF[7]
    }}

if(APPNUM == 1){
  PSTMSP[1] = APPRAT
}else{
for(I in 2:APPNUM){
  PSTMSP[I] = PSTMSP[I-1] * exp(-KDEGP*APSPAC) + APPRAT
}
}

KADS1 = (9.2529+1.751*KOC) / (1.341E6+KOC)
KADS1 = 0.12 * KADS1
SDINIT=(1.123206*PSTMSP[APPNUM] * DRIFT * WBAREA * exp(-KADS1))/20
LOOK1 = PSTMSP[APPNUM]
LOOK2 = exp(-KADS1)
KADSUS = (9366.5+12.4*KOC) / (655000+KOC)
KADSUS = 0.12 * KADSUS

for(I in 1:100){
  ADSFRS[I] = exp(-KADSUS*I)
}

SDFIN = 1.123206 * PSTMSP[APPNUM] * DRIFT * ((37.0388+9E-6*KOC) / (750+KOC))

for(I in 1:99){
  SDCONC[I] = (SDFIN + ADSFRS[I] * (SDINIT-SDFIN)) * exp(-KDEGP*I)
  SCON1 = SDCONC[1]
  SCON2 = SDCONC[2]
  SCON3 = SDCONC[3]
  SCON4 = SDCONC[4]
  SCON5 = SDCONC[5]
  SCON6 = SDCONC[6]
  SCON7 = SDCONC[7]
  SCON8 = SDCONC[8]
  SCON9 = SDCONC[9]
  SCON10 = SDCONC[10]
}

ROINIT = (1.123206 * PSTMSF[APPNUM] * APPEFF * ROAREA * PCTSRO * KDFRAC * DEGFRF[STORM] / INCORP) / 20
KADSUR = (5742.9+7.6*KOC) / (405000+KOC)
KADSUR = 0.12 * KADSUR

for(I in 1:100){
  ADSFRR[I] = exp(-KADSUR*I)
}

ROFIN = 1.123206 * PSTMSF[APPNUM] * APPEFF * PCTSRO * ROAREA * DEGFRF[STORM] * ((157.845+4.3E-6*KOC**1.215) /
        (510+KOC**1.215)) / INCORP / 6.262

for(I in 1:100){
  ROCONC[I] = (ROFIN + ADSFRR[I] * (ROINIT-ROFIN)) * exp(-KDEGP*I)
  ROCO1 = ROCONC[1]
  ROCO2 = ROCONC[2]
  ROCO3 = ROCONC[3]
  ROCO4 = ROCONC[4]
  ROCO5 = ROCONC[5]
  ROCO6 = ROCONC[6]
  ROCO7 = ROCONC[7]
  ROCO8 = ROCONC[8]
  ROCO9 = ROCONC[9]
  ROCO10 = ROCONC[10]
}

sigDigits <- function(x){
  z <- as.numeric(format(x,digits=5, nsmall=5))
  z <- trunc(z*10000)/10000
  z
}
sigDigits2 <- function(x){
  z <- as.numeric(format(x,digits=5, nsmall=3))
  z <- trunc(z*1000)/1000
  z
}
dig <- function(x, k) as.numeric(as.character(trimws(format(round(x, k), nsmall=k))))

if(METHOD == 'A' | METHOD == 'a' | METHOD == 'B' | METHOD == 'b' | METHOD == 'C' | METHOD == 'c'){
  CONC0 = ROINIT + SDCONC[STORM]

  for(I in 1:99){
    CHRONIC[I] = ROCONC[I] + SDCONC[I+STORM-1]
  }

} else if (METHOD == 'D' | METHOD == 'd'){
  CONC0 = ROINIT
  for(I in 1:99){
    CHRONIC[I] = ROCONC[I]
  }
}
#############################################################
SUM4 = 0.0
SUM21 = 0.0
SUM60 = 0.0
SUM90 = 0.0

for(I in 1:3){
  SUM4 = SUM4 + CHRONIC[I]
}

CONC4 = (CONC0 + SUM4) / 4

for(I in 1:20){
  SUM21 = SUM21 + CHRONIC[I]
}

CONC21 = (CONC0 + SUM21) / 21

for(I in 1:59){
  SUM60 = SUM60 + CHRONIC[I]
}

CONC60 = (CONC0 + SUM60) / 60

for(I in 1:89){
  SUM90 = SUM90 + CHRONIC[I]
}

CONC90 = (CONC0 + SUM90) / 90

if(KD >= 10.0) {
  CONC0 = CONC0 / log10(KD)
  CONC4 = CONC4 / log10(KD)
  CONC21 = CONC21 / log10(KD)
  CONC60 = CONC60 / log10(KD)
  CONC90 = CONC90 / log10(KD)
}

if(CONC0 >= SOL) {CONC0 = SOL}
if(CONC4 >= SOL) {CONC4 = SOL}
if(CONC21 >= SOL) {CONC21 = SOL}
if(CONC60 >= SOL) {CONC60 = SOL}
if(CONC90 >= SOL) {CONC90 = SOL}

# if(CONC0 >= 1.0){
#   UNITS = 'MILLIGRAMS/LITER (PPM)'
# }
# 
# if(CONC0 < 1.0 & CONC0 >= 0.001){
#   CONC0 = CONC0 * 1000
#   CONC4 = CONC4 * 1000
#   CONC21 = CONC21 * 1000
#   CONC60 = CONC60 * 1000
#   CONC90 = CONC90 * 1000
#   UNITS = 'MICROGRAMS/LITER (PPB)'
# }
# 
# if(CONC0 < 0.001){
#   CONC0 = CONC0 * 1000000
#   CONC4 = CONC4 * 1000000
#   CONC21 = CONC21 * 1000000
#   CONC60 = CONC60 * 1000000
#   CONC90 = CONC90 * 1000000
#   UNITS = 'NANOGRAMS/LITER (PPTr)'
# }

# if(SOL >= 1.0){
#   SOLUNITS = 'PPM'
# }
# 
# if(SOL < 1.0 & SOL >= 0.001) {
#   SOL = SOL * 1000
#   SOLUNITS = 'PPB'
# }
# 
# if(SOL < 0.001) {
#   SOL = SOL * 1000000
#   SOLUNITS = 'PPTr'
# }

########################
# Start writing to an output file
 # OUTFIL="out_geneec.txt"
 # sink(OUTFIL, append = T)

# if(APFLAG == 1) {INCORP = 0.0}
#
# if(ADSORP == 'A' | ADSORP == 'a'){
#   cat(sprintf("\n   RUN No. %3.0f FOR  %s   ON  %s   * INPUT VALU 2ES * ", CODE,CHMNAM,CROP))
#   cat('\n   --------------------------------------------------------------------')
#   cat('\n   RATE (#/AC)   No.APPS &   SOIL  SOLUBIL  APPL TYPE  NO-SPRAY INCORP')
#   cat("\n    ONE(MULT)    INTERVAL    Koc   (  %s )   (%DRIFT)    (FT)    (IN)", SOLUNITS)
#   cat('\n   --------------------------------------------------------------------\n')
#   cat(sprintf(" %7.3f%s%7.3f%s  %d %d %8.1f %7.1f   %s%s%5.1f%s %5.1f  %4.1f",
#               APPRAT,'(',PSTMSF[APPNUM],')',APPNUM,APSPAC,KOC,SOL,SPTYPE,'(',DRIFT*100,')',
#               YLOCEN,INCORP,"\n"))
#
# } else if (ADSORP == 'B' | ADSORP == 'b'){
#   cat(sprintf("   RUN No. %3.0f FOR  %s   ON  %s   * INPUT VALU 2ES * ", CODE,CHMNAM,CROP))
#   cat('\n   --------------------------------------------------------------------')
#   cat('\n   RATE (#/AC)   No.APPS &   SOIL  SOLUBIL  APPL TYPE  NO-SPRAY INCORP')
#   cat("\n    ONE(MULT)    INTERVAL    Kd    (  %s )   (%DRIFT)  ZONE(FT)  (IN)", SOLUNITS)
#   cat('\n   --------------------------------------------------------------------\n')
#   cat(sprintf(" %7.3f%s%7.3f%s  %d %d  %8.1f %7.1f   %s%s%5.1f%s %5.1f  %4.1f", APPRAT,
#               '(',PSTMSF[APPNUM],')',APPNUM,APSPAC,KD,SOL,SPTYPE,'(',DRIFT*100,')',
#               YLOCEN,INCORP,"\n"))
# }
#
# title2 = paste(
#   "\n\n\n   FIELD AND STANDARD POND HALFLIFE VALUES (DAYS)
#    --------------------------------------------------------------------
#    METABOLIC  DAYS UNTIL  HYDROLYSIS   PHOTOLYSIS   METABOLIC  COMBINED
#    (FIELD)  RAIN/RUNOFF    (POND)      (POND-EFF)    (POND)   	(POND)
#    --------------------------------------------------------------------\n", sep = "\n")
#
# if(APFLAG == 1) {INCORP = 0.0}
#
# if(HYDHAP <= 0.0){
#   cat(sprintf(title2,"\n"))
#   cat(sprintf("   %7.2f     %2d          %s       %6.2f%s%8.2f %6.2f   %7.2f", METHAF,STORM-1,'N/A ',FOTHAP,'-',FOTHAP*124,METHAP,DEGHAP))
# } else {
#   cat(sprintf(title2,"\n"))
#   cat(sprintf("   %7.2f     %2d        %7.2f  %6.2f%s%8.2f %6.2f   %7.2f", METHAF,STORM-1,HYDHAP,FOTHAP,'-',FOTHAP*124,METHAP,DEGHAP))
# }
# #
# # close(file("analysis-output.txt", open="w" ) )
# # sink()
# 
# 
# if(l==2){
# cat(sprintf("   GENERIC EECs (GENEEC) (units: PPM) Version 2.0 Aug 1 2001\n"))
# cat("   --------------------------------------------------------------------
#        PEAK      MAX 4 DAY     MAX 21 DAY    MAX 60 DAY    MAX 90 DAY    
#        GEEC      AVG GEEC       AVG GEEC      AVG GEEC      AVG GEEC
#     ----------------------------------------------------------------------\n")
# cat(sprintf("     %7.2f     %7.2f     %7.2f      %7.2f       %7.2f     \n",CONC0,CONC4,CONC21,CONC60,CONC90))
# }else{
# cat(sprintf("     %7.2f     %7.2f     %7.2f      %7.2f       %7.2f     \n",CONC0,CONC4,CONC21,CONC60,CONC90))
# }
# sink()

CONC0_df = rbind(CONC0_df, CONC0)
CONC4_df = rbind(CONC4_df, CONC4)
CONC21_df = rbind(CONC21_df, CONC21)
CONC60_df = rbind(CONC60_df, CONC60)
CONC90_df = rbind(CONC90_df, CONC90)

CODE_df <- rbind(CODE_df, CODE)
CHMNAM_df <- rbind(CHMNAM_df, CHMNAM)
CROP_df <- rbind(CROP_df, CROP)
}

res_df = data.frame(CODE_df,
                    CROP_df,
                    CHMNAM_df,
                    CONC0_df*1000,
                    CONC4_df*1000,
                    CONC21_df*1000,
                    CONC60_df*1000,
                    CONC90_df*1000)

names(res_df) = c("Run", "Crop", "Product", "Peak", "Max4", "Max21", "Max60", "Max90")

#res_df2=format.df(res_df, digits=4, numeric.dollar=F)
#res_df2= data.frame(res_df2)

return(list(res_df))
}








