import pandas as pd

spotify_df = pd.read_csv('data/spotify.csv')
yt_df = pd.read_csv('data/yt.csv')


# ----- Split the Spotify data -----
spotify_colnames = list(spotify_df.columns)[1:]
def split_spotify_df(variable):
    treatment, values, variables, app = [], [], [], []
    for col in spotify_colnames:
        if variable in col:
            if variable == "high" and "veryhigh" in col:
                continue
            treatment.extend([col for _ in range(30)])
            values.extend(list(spotify_df[col])[0:-1])
            app.extend(["Spotify" for _ in range(30)])
            variables.extend([variable for _ in range(30)])
            
    return create_df(app, treatment, values, variables)


# ----- Split the YouTube Music data -----
yt_colnames = list(yt_df.columns)[1:]
def split_yt_df(variable):
    treatment, values, variables, app = [], [], [], []
    for col in yt_colnames:
        if variable in col:
            treatment.extend([col for _ in range(30)])
            values.extend(list(yt_df[col])[0:-1])
            app.extend(["YouTube Music" for _ in range(30)])
            variables.extend([variable for _ in range(30)])              
    
    return create_df(app, treatment, values, variables)  


# ----- Generate the dataframe -----
def create_df(app, treatment, values, variables): 
    result_df = pd.DataFrame(columns=["app", "variable", "treatment", "value"])
    result_df["app"] = app
    result_df["treatment"] = treatment
    result_df["value"] = values
    result_df["variable"] = variables
    return result_df


# ----- Combine two dataframes -----
def output_csv(variable):
    output_df = pd.concat([split_spotify_df(variable), split_yt_df(variable)], axis=0)
    output_df.to_csv(f'data/{variable}.csv', index=False) 


variables = ["download", "wifi", "v1", "v2", "v3", "low", "normal", "high", "veryhigh"]
for v in variables:
    output_csv(v)