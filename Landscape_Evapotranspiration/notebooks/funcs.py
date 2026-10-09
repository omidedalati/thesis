import numpy as np
import pandas as pd
from datetime import datetime
from IPython.display import display

def data_cleansing(data, location_name):
       
    # جایگزینی کاراکترهای عربی
    data = data.replace('ئ', 'ی', regex=True).replace('ي', 'ی', regex=True) 
    location_name = location_name.replace('ئ', 'ی', regex=True).replace('ي', 'ی', regex=True)

    data = data.rename(columns={'پیمان': 'نام پیمان'})

    data = pd.merge(
        left=data,
        right=location_name[["نام", "منطقه", "ناحیه", "پیمان"]],
        how="left",
        left_on="نام پیمان",
        right_on="نام",
    ).drop("نام", axis=1)

    data["منطقه"] = data["منطقه"].astype(str).str.zfill(2)
    data["ناحیه"] = data["ناحیه"].astype(str).str.zfill(2)
    data["پیمان"] = data["پیمان"].astype(str).str.zfill(2)
    
    data_duplicates = data[data.duplicated(subset=list(data.columns)[1:], keep=False)]

    data = data.drop_duplicates(subset=list(data.columns)[1:], keep="first")

    # EXTRACT ADDRESS
    # به‌روزرسانی برای پانداس ۲: تخصیص مستقیم نام به ستون ایجاد شده
    tmp = data.groupby(["منطقه", "ناحیه", "پیمان"])["نام لکه"].value_counts(dropna=False, sort=False).reset_index(name="تعداد")

    Address = []

    for R in list(tmp["منطقه"].unique()):
        tmpR = tmp[tmp["منطقه"] == R]
        for D in list(tmpR["ناحیه"].unique()):
            tmpD = tmpR[tmpR["ناحیه"] == D]
            for P in list(tmpD["پیمان"].unique()):
                tmpP = tmpD[tmpD["پیمان"] == P]
                Address += list(range(1, len(tmpP) + 1))

    tmp["آدرس"] = Address
    tmp["آدرس"] = tmp["آدرس"].astype(str).str.zfill(3)

    tmp = tmp[["منطقه", "ناحیه", "پیمان", "نام لکه", "آدرس"]]

    data = pd.merge(left=data,
                    right=tmp,
                    how="left",
                    on=["منطقه", "ناحیه", "پیمان", "نام لکه"])

    data = data.sort_values(['منطقه', 'ناحیه', 'پیمان', 'آدرس'])

    # METER ERROR MODIFY
    data_meters_errors = data[(data["نوع قلم"] == "درخت و درختچه") & (data["زیرمجموعه هر قلم"] == "متر")]

    data = data.drop(data[(data["نوع قلم"] == "درخت و درختچه") & (data["زیرمجموعه هر قلم"] == "متر")].index)

    # SPOT CLASS
    SPOT_CLASS = {
        "میادین": "01",
        "لچکی ها": "02",
        "آیلند های بزرگراه": "03",
        "آیلند ها": "04",
        "حاشیه های بزرگراه": "05",
        "حاشیه معابر": "06",
        "بوستان خطی": "07",
        "پارک های زیر 6 هکتار": "08",
        "پارک های بین 6 تا 10 هکتار": "09",
        "پارک های بالای 10 هکتار": "10",
        "جنگل کاری داخل محدوده": "11",
        "کمربندی": "12",
        "کمربند سبز حفاظتی": "13",
    }

    # روش امن جایگزینی ستون‌ها برای جلوگیری از خطای KeyError
    tmp = data["نوع لکه"].value_counts(dropna=False, sort=True).reset_index()
    tmp.columns = ["نام نوع لکه", "تعداد رکورد"]
    tmp["نوع لکه"] = list(map(SPOT_CLASS.get, tmp["نام نوع لکه"]))
    tmp = tmp[["نوع لکه", "نام نوع لکه", "تعداد رکورد"]]

    data.rename(columns={'نوع لکه': 'نام نوع لکه'}, inplace=True)

    data = pd.merge(
        left=data,
        right=tmp[["نوع لکه", "نام نوع لکه"]],
        how="left",
        on=["نام نوع لکه"]
    )

    # ITEM CLASS
    Item_Class = {
        "چمن": "01",
        "گل دایم باغچه های معمولی": "02",
        "گل دایم فلاورباکسهای سطوح شیب دار": "03",
        "گل فصل باغچه های معمولی": "04",
        "گل فصل فلاورباکس های سطوح شیب دار": "05",
        "پرچین": "06",
        "درخت و درختچه": "07",
        "درختان جنگلی": "08",
        "حفظ و نگهداری چمن": "51",
        "حفظ و نگهداری گل دایمی": "52",
        "حفظ و نگهداری سطوح شیبدار گل دایمی": "53",
        "حفظ و نگهداری گل فصلی": "54",
        "حفظ و نگهداری سطوح شیبدار گل فصلی": "55",
        "حفظ و نگهداری پرچین": "56",
        "حفظ و نگهداری درخت و درختچه": "57",
        "معابر": "91",
        "محوطه بازی كودكان": "92",
        "فضاهای مسقف": "93",
        "آبنما نظافت": "94"
    }

    tmp = data["نوع قلم"].value_counts(dropna=False, sort=True).reset_index()
    tmp.columns = ["نام نوع قلم", "تعداد رکورد"]
    tmp["نوع قلم"] = list(map(Item_Class.get, tmp["نام نوع قلم"]))
    tmp = tmp[["نوع قلم", "نام نوع قلم", "تعداد رکورد"]].sort_values(["نوع قلم"]).reset_index(drop=True)
    tmp = tmp[["نوع قلم", "نام نوع قلم"]]

    data.rename(columns={'نوع قلم' : 'نام نوع قلم'}, inplace=True)

    data = pd.merge(
        left=data,
        right=tmp,
        how="left",
        on=["نام نوع قلم"]
    )

    # SUBITEM CLASS
    Subitem_Class = {
        "آبیاری ثقلی": "01",
        "آبیاری تانکری": "02",
        "آبیاری شلنگی": "03",
        "آبیاری تحت فشار": "04",
        "سایر گل های دایمی": "05",
        "گل رز": "06",
        "متر مربع": "07",
        "اصله": "08",
    }

    tmp = data["زیرمجموعه هر قلم"].value_counts(dropna=False, sort=True).reset_index()
    tmp.columns = ["نام زیرمجموعه هر قلم", "تعداد رکورد"]
    tmp["زیرمجموعه هر قلم"] = list(map(Subitem_Class.get, tmp["نام زیرمجموعه هر قلم"]))
    tmp = tmp[["زیرمجموعه هر قلم", "نام زیرمجموعه هر قلم"]].sort_values(["زیرمجموعه هر قلم"]).reset_index(drop=True)

    data.rename(columns={'زیرمجموعه هر قلم': 'نام زیرمجموعه هر قلم'}, inplace=True)

    data = pd.merge(
        left=data,
        right=tmp[["زیرمجموعه هر قلم", "نام زیرمجموعه هر قلم"]],
        how="left",
        on=["نام زیرمجموعه هر قلم"]
    )

    # ID GENERATE
    data["شناسه"] = (
            data["منطقه"]
            + "."
            + data["ناحیه"]
            + "."
            + data["پیمان"]
            + "."
            + data["آدرس"]
            + "."
            + data["نوع لکه"]
            + "-"
            + data["نوع قلم"]
            + "."
            + data["زیرمجموعه هر قلم"]
    )

    # DROP NA ID
    data = data.dropna(subset=["شناسه"])

    # DUPLICATE ID
    data_duplicates_id = data[data.duplicated(subset="شناسه", keep=False)] 
    
    tmp = data.groupby(["شناسه"]).agg({"نمایش آخرین ریزمتره (ریزمتره نهایی)": "sum",
                                        "مقدار در صورتجلسه تحویل و تحول": "sum"}).reset_index()

    data = data.drop_duplicates(subset="شناسه", keep="first")

    data = pd.merge(
        left=data,
        right=tmp,
        how="left",
        on="شناسه").drop(["مقدار در صورتجلسه تحویل و تحول_x", "نمایش آخرین ریزمتره (ریزمتره نهایی)_x"],
                         axis=1)

    data = data.rename(
        columns={"نمایش آخرین ریزمتره (ریزمتره نهایی)_y": "نمایش آخرین ریزمتره (ریزمتره نهایی)",
                 "مقدار در صورتجلسه تحویل و تحول_y": "مقدار در صورتجلسه تحویل و تحول"}
    )

    # RENAME COLUMNS NAME
    data.rename(
        columns={
            "نمایش آخرین ریزمتره (ریزمتره نهایی)": "مقدار آیتم",
            "مساحت لکه (مترمربع)": "مساحت آدرس",
            "مساحت پیمان (مترمربع)": "مساحت پیمان",
            "مقدار در صورتجلسه تحویل و تحول": "مقدار آیتم خام",
        },
        inplace=True,
    )

    data = data[
        [
            "شناسه",
            "منطقه",
            "ناحیه",
            "پیمان",
            "آدرس",
            "نوع لکه",
            "نوع قلم",
            "زیرمجموعه هر قلم",
            "مقدار آیتم",
            "مساحت آدرس",
            "مساحت پیمان",
            "نوع آیتم",
            "نام پیمان",
            "نام لکه",
            "نام نوع لکه",
            "نام نوع قلم",
            "نام زیرمجموعه هر قلم",
            "مقدار آیتم خام"
        ]
    ]
    
    data['مقدار آیتم خام'] = data['مقدار آیتم']

    data.sort_values(by=['منطقه', 'ناحیه', 'پیمان', 'آدرس'], ascending=True, inplace=True)

    data.reset_index(drop=True, inplace=True)

    return data, data_duplicates, data_meters_errors, data_duplicates_id


def tree_area_modify(data):

    result = pd.DataFrame(columns=data.columns)

    for idf, df in data.groupby(["منطقه", "ناحیه", "پیمان", "آدرس"]):
        df = df.reset_index(drop=True)
        TI_Check = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]').shape[0]
        TI_Index = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]').index.to_list()

        if TI_Check > 0:
            
            tree_area = 9
            
            if df[df["زیرمجموعه هر قلم"] == '08'].shape[0] > 0:
                if df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01"]').shape[0] > 0:
                    Asle_Tree = df[df['زیرمجموعه هر قلم'] == '08'].iloc[0]["مقدار آیتم"]
                    Independent_Tree_noGravity = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["02", "03", "04"]')["مقدار آیتم"].sum()
                    df.loc[(df['نوع قلم'] == "07") & (df['زیرمجموعه هر قلم'] == "01"), 'مقدار آیتم'] = Asle_Tree - Independent_Tree_noGravity
                    Independent_Tree = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]')["مقدار آیتم"].sum()
                else:
                    Asle_Tree = df[df['زیرمجموعه هر قلم'] == '08'].iloc[0]["مقدار آیتم"]
                    Independent_Tree = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]')["مقدار آیتم"].sum()
            else:
                Asle_Tree = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]')["مقدار آیتم"].sum()
                Independent_Tree = df.query('`نوع قلم` == "07" and `زیرمجموعه هر قلم` == ["01", "02", "03", "04"]')["مقدار آیتم"].sum()
                
            Address_Area = df[df["نوع آیتم"] == "ریزآیتمی"].iloc[0]["مساحت آدرس"]
            
            Spot_Class = df.iloc[0]["نوع لکه"]
            
            if Spot_Class in ['07', '08', '09', '10', '11', '12', '13']:
                Tree_Area = Address_Area * 1.00 / Asle_Tree
                tree_area = 9 if Tree_Area > 9 else Tree_Area
            elif Spot_Class in ['01', '02']:
                Tree_Area = Address_Area * 1.00 / Asle_Tree
                tree_area = 9 if Tree_Area > 9 else Tree_Area
            elif Spot_Class in ['03', '04', '05', '06']:
                Tree_Area = Address_Area * 1.00 / Asle_Tree
                tree_area = 9 if Tree_Area > 9 else Tree_Area                

            for i in TI_Index:
                df.loc[i, "مقدار آیتم"] = df.loc[i, "مقدار آیتم"] * tree_area

            # استفاده از .copy() برای جلوگیری از SettingWithCopyWarning
            tmp = df.iloc[[0], :].copy()
            tmp.loc[0, "نوع قلم"] = "07"
            tmp.loc[0, "زیرمجموعه هر قلم"] = "09"
            tmp.loc[0, "شناسه"] = (tmp.loc[0, "منطقه"] + "." + tmp.loc[0, "ناحیه"] + "." + tmp.loc[0, "پیمان"] + "."
                                   + tmp.loc[0, "آدرس"] + "." + tmp.loc[0, "نوع لکه"] + "-" + tmp.loc[
                                       0, "نوع قلم"] + "." + tmp.loc[0, "زیرمجموعه هر قلم"])
            tmp.loc[0, "مقدار آیتم"] = Independent_Tree
            tmp.loc[0, "مقدار آیتم خام"] = Independent_Tree
            tmp.loc[0, "نام نوع قلم"] = "درخت و درختچه"
            tmp.loc[0, "نام زیرمجموعه هر قلم"] = "اصله مستقل"
            tmp.loc[0, "نوع آیتم"] = "ریزآیتمی"
            tmp.loc[0, "مساحت آدرس"] = df[df["نوع آیتم"] == "ریزآیتمی"].iloc[0]["مساحت آدرس"]
            tmp.loc[0, "مساحت پیمان"] = df[df["نوع آیتم"] == "ریزآیتمی"].iloc[0]["مساحت پیمان"]

            df = pd.concat([df, tmp])
            result = pd.concat([result, df])
        else:
            result = pd.concat([result, df])

    result = result.reset_index(drop=True)

    return result


def report_area(data):

    tmp1 = data.pivot_table(
        values=["مقدار آیتم"],
        index=["منطقه", "ناحیه", "پیمان", "آدرس"],
        columns=["نام نوع قلم", "نام زیرمجموعه هر قلم"],
        aggfunc={'مقدار آیتم': np.sum}
    )

    # مسطح کردن ساختار MultiIndex قبل از ادغام برای جلوگیری از MergeError
    tmp1.columns = [' '.join(col).replace('مقدار آیتم', '').strip() for col in tmp1.columns.values]

    tmp2 = data.pivot_table(
        values=["مساحت آدرس"],
        index=["منطقه", "ناحیه", "پیمان", "آدرس"],
        columns=["نوع آیتم"],
        aggfunc={'مساحت آدرس': np.mean}
    )

    # مسطح کردن ساختار MultiIndex قبل از ادغام
    tmp2.columns = [' '.join(col).strip() for col in tmp2.columns.values]

    tmp = pd.merge(
        left=tmp1.fillna(0),
        right=tmp2.fillna(0),
        how="outer",
        left_index=True,
        right_index=True
    )

    tmp_counts = data.groupby(["منطقه", "ناحیه", "پیمان", "آدرس", "نام نوع لکه", "نام لکه"]).size().reset_index(name='count')
    tmp_counts = tmp_counts.drop(columns=['count']).set_index(["منطقه", "ناحیه", "پیمان", "آدرس"])

    tmp = pd.merge(
        left=tmp,
        right=tmp_counts,
        how="outer",
        left_index=True,
        right_index=True
    )

    tmp["مجموع سطوح غیر سبز"] = tmp["آبنما نظافت متر مربع"] + tmp["فضاهای مسقف متر مربع"] + tmp["محوطه بازی كودكان متر مربع"] + tmp["معابر متر مربع"]
    tmp["مجموع سطوح سبز به غیر از درخت و درختچه"] = tmp["پرچین متر مربع"] + tmp["چمن متر مربع"] + tmp["گل دایم باغچه های معمولی متر مربع"] + tmp["گل دایم فلاورباکسهای سطوح شیب دار متر مربع"] + tmp["گل فصل باغچه های معمولی متر مربع"] + tmp["گل فصل فلاورباکس های سطوح شیب دار متر مربع"]
    tmp["مجموع سطوح"] = tmp["مجموع سطوح غیر سبز"] + tmp["مجموع سطوح سبز به غیر از درخت و درختچه"] + tmp["درخت و درختچه متر مربع"]
    tmp["مجموع سطوح آبیاری تانکری"] = tmp[[col for col in tmp.columns if 'تانکری' in col]].sum(axis=1)
    tmp["مجموع سطوح آبیاری تحت فشار"] = tmp[[col for col in tmp.columns if 'تحت فشار' in col]].sum(axis=1)
    tmp["مجموع سطوح آبیاری ثقلی"] = tmp[[col for col in tmp.columns if 'ثقلی' in col]].sum(axis=1)
    tmp["مجموع سطوح آبیاری شلنگی"] = tmp[[col for col in tmp.columns if 'شلنگی' in col]].sum(axis=1)
    tmp["مجموع کل مساحت تحت آبیاری"] = tmp["مجموع سطوح آبیاری تانکری"] + tmp["مجموع سطوح آبیاری تحت فشار"] + tmp["مجموع سطوح آبیاری شلنگی"] + tmp["مجموع سطوح آبیاری ثقلی"]
    
    return tmp



def prepare_coefficient(data, raw_coeff, Method_Selected="Municipality"):

    Month_Name = ["M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12"]
    n_Day = [31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29]

    Kd_v2 = {
        1: 1,
        2: 1,
        3: 1.2,
        4: 1.2,
        5: 1.3,
        6: 1.3,
        7: 1.3,
        8: 1.3
    }

    Items = ["01", "02", "03", "04", "05", "06", "07", "08"]
    Subitems = ["01", "02", "03", "04", "08", "09"]

    dataset = data.query('`نوع قلم` == @Items')\
        .query('`زیرمجموعه هر قلم` == @Subitems')\
            .reset_index(drop=True)

    PET = raw_coeff['PET'].set_index('Method').loc[Method_Selected,:].tolist()

    Effe_Precip = raw_coeff['Effe_Precip'].set_index('Method').loc[Method_Selected,:].tolist()

    Ks = raw_coeff['Ks']
    Ks["کد نوع گونه"] = Ks["کد نوع گونه"].astype(str).str.zfill(2)
    Ks = pd.melt(
        Ks,
        id_vars=["نوع گونه", "کد نوع گونه"],
        var_name="ماه",
        value_name="Ks"
    )

    Kd = raw_coeff['Kd']
    Kd["کد نوع گونه"] = Kd["کد نوع گونه"].astype(str).str.zfill(2)
    Kd["کد نوع لکه"] = Kd["کد نوع لکه"].astype(str).str.zfill(2)
    Kd = pd.melt(
        Kd,
        id_vars=["نوع گونه", "کد نوع گونه", "نوع لکه", "کد نوع لکه"],
        var_name="ماه",
        value_name="Kd"
    )

    Kmc = raw_coeff['Kmc']
    Kmc["کد نوع لکه"] = Kmc["کد نوع لکه"].astype(str).str.zfill(2)

    IE = raw_coeff['IE']
    IE["کد روش آبیاری"] = IE["کد روش آبیاری"].astype(str).str.zfill(2)
    IE["کد نوع گونه"] = IE["کد نوع گونه"].astype(str).str.zfill(2)
    IE["کد نوع لکه"] = IE["کد نوع لکه"].astype(str).str.zfill(2)


    Coeff = Kd.merge(
        right=Kmc,
        how="outer",
        on=["نوع لکه", "کد نوع لکه"]
    ).merge(
        right=Ks,
        how="outer", 
        on=["نوع گونه", "کد نوع گونه", "ماه"]
    ).merge(
        right=IE, 
        how="outer", 
        on=["نوع گونه", "کد نوع گونه", "نوع لکه", "کد نوع لکه"]
    )

    Coeff = Coeff[[
        "نوع لکه",
        "کد نوع لکه",
        "نوع گونه",
        "کد نوع گونه",
        "روش آبیاری",
        "کد روش آبیاری",
        "ماه",
        "Ks",
        "Kd",
        "Kmc",
        "IE"
    ]]

    Coeff.columns = [
        "نام نوع لکه",
        "نوع لکه",
        "نام نوع قلم",
        "نوع قلم",
        "نام زیرمجموعه هر قلم",
        "زیرمجموعه هر قلم",
        "ماه",
        "Ks",
        "Kd",
        "Kmc",
        "IE"
    ]

    Coeff = Coeff.pivot(
        index=["نام نوع لکه", "نوع لکه", "نام نوع قلم", "نوع قلم", "نام زیرمجموعه هر قلم", "زیرمجموعه هر قلم"],
        columns='ماه', 
        values=["Ks", "Kd", "Kmc", "IE"]
    ).reset_index()

    Coeff.columns = ['_'.join(col).strip() for col in Coeff.columns.values]

    Coeff.rename(
        columns={
            "نام نوع لکه_": "نام نوع لکه",
            "نوع لکه_" : "نوع لکه",
            "نام نوع قلم_" : "نام نوع قلم",
            "نوع قلم_" : "نوع قلم",
            "نام زیرمجموعه هر قلم_" : "نام زیرمجموعه هر قلم",
            "زیرمجموعه هر قلم_": "زیرمجموعه هر قلم"
        },
        inplace=True
    )

    IDs_Asle = ["08", "09"]
    dataset_coeff = pd.DataFrame()

    for idf, df in dataset.groupby(["منطقه", "ناحیه", "پیمان", "آدرس"]):

        if df.query('`زیرمجموعه هر قلم` != @IDs_Asle').shape[0] != 0:        

            # به‌روزرسانی برای جلوگیری از مشکل اعمال توابع روی GroupBy در نسخه‌های جدید
            tmp = df.query('`زیرمجموعه هر قلم` != @IDs_Asle')\
            .groupby(["منطقه", "ناحیه", "پیمان", "آدرس", "نوع قلم"])["مقدار آیتم"]\
            .sum().reset_index(name="مساحت گونه")\
            .assign(**{"درصد مساحت گونه در لکه" : lambda x : x['مساحت گونه'] / x['مساحت گونه'].sum()})

            df = df.merge(tmp, how='left', on=["منطقه", "ناحیه", "پیمان", "آدرس", "نوع قلم"])

            df.loc[df["زیرمجموعه هر قلم"] == "08", ["مساحت گونه", "درصد مساحت گونه در لکه"]] = 0
            df.loc[df["زیرمجموعه هر قلم"] == "09", ["مساحت گونه", "درصد مساحت گونه در لکه"]] = 0

            Number_Species = tmp["نوع قلم"].unique().__len__()

            if "08" in list(df["زیرمجموعه هر قلم"]):
                Number_Species += 1

            df["تعداد گونه در لکه"] = Number_Species

            df['Kd_v2'] = list(map(Kd_v2.get, df['تعداد گونه در لکه']))

            df = df.merge(Coeff.drop(["نام نوع لکه", "نام نوع قلم", "نام زیرمجموعه هر قلم"], axis=1),
                        how="left",
                        on=["نوع لکه", "نوع قلم", "زیرمجموعه هر قلم"])

            dataset_coeff = pd.concat([dataset_coeff, df])
        else:
            df["مساحت گونه"] = df["مقدار آیتم"] * 9
            df = df.assign(**{"درصد مساحت گونه در لکه" : 1})
            df = df.assign(**{"تعداد گونه در لکه" : 1})
            df = df.assign(**{"Kd_v2" : list(map(Kd_v2.get, df['تعداد گونه در لکه']))})

            df = df.merge(Coeff.drop(["نام نوع لکه", "نام نوع قلم", "نام زیرمجموعه هر قلم"], axis=1),
                        how="left",
                        on=["نوع لکه", "نوع قلم", "زیرمجموعه هر قلم"])

            dataset_coeff = pd.concat([dataset_coeff, df])

    dataset_coeff = dataset_coeff.reset_index(drop=True)

    for i, value in enumerate(Month_Name):
        dataset_coeff["PET_" + value] = PET[i]

    for i, value in enumerate(Month_Name):
        dataset_coeff["Effe_Precip_" + value] = Effe_Precip[i]

    for i, value in enumerate(Month_Name):
        dataset_coeff["n_Day_" + value] = n_Day[i]

    return dataset_coeff