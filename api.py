import requests
import pandas as pd
import matplotlib.pyplot as plt

BASE_URL = "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json"

headers = {
    "Authorization": "Apikey d72beb4f3da9217d10dcee4bf3d19e8ce7ebd2bb158a6acba1ef4810",
    "accept": "application/json"
}

def get_series(series_key):
    params = {
        "select": "time_period_end,obs_value",
        "where": f"series_key='{series_key}'",
        "order_by": "time_period_end"
    }
    
    response = requests.get(BASE_URL, headers=headers, params=params)
    
    if response.status_code == 200:
        df = pd.DataFrame(response.json())
        df["time_period_end"] = pd.to_datetime(df["time_period_end"])
        df["obs_value"] = pd.to_numeric(df["obs_value"])
        return df
    else:
        print("Erreur :", response.status_code)
        return None

# OAT 10 ans
series_key = "FM.M.FR.EUR.FR2.BB.FR10YT_RR.YLD"

df_oat10 = get_series(series_key)

if df_oat10 is not None:
    df_oat10 = df_oat10[df_oat10["time_period_end"] >= "2000-01-01"]
    
    plt.figure(figsize=(10,6))
    plt.plot(df_oat10["time_period_end"], df_oat10["obs_value"])
    plt.title("OAT France 10 ans")
    plt.xlabel("Date")
    plt.ylabel("Taux (%)")
    plt.grid()
    plt.show()
