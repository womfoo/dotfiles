import netrc
import requests
from datetime import datetime, timedelta
import pytz


def get_github_credentials():
    netrc_file = netrc.netrc()
    auth1 = netrc_file.authenticators("github.com")
    auth2 = netrc_file.authenticators("api.github.com")

    username1, _, password1 = auth1
    username2, _, password2 = auth2

    if username1 == username2 and password1 == password2:
        return username1, password1
    else:
        raise Exception("GitHub invalid or not present")


def query_github_api():
    try:
        username, token = get_github_credentials()
        api_url = f"https://api.github.com/users/{username}"
        headers = {"Authorization": "Bearer " + token}
        response = requests.get(api_url, params=headers, timeout=1.5)

        if response.status_code == 200:
            if "github-authentication-token-expiration" in response.headers:
                expiration_str = response.headers[
                    "github-authentication-token-expiration"
                ]
                expiration_time = datetime.strptime(
                    expiration_str, "%Y-%m-%d %H:%M:%S %z"
                )
                current_time = datetime.now(pytz.utc)
                time_left = expiration_time - current_time
                print("Token Expiration:", expiration_str)
                print("Time left:", str(time_left))
        else:
            print(
                f"Failed to fetch data from github. Status Code: {response.status_code} check tokens at https://github.com/settings/tokens"
            )
    except Exception as e:
        print(f"Error: {str(e)}")


query_github_api()
