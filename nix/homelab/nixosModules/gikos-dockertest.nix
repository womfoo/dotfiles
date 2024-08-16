{ ... }:
{
  users.extraUsers.dockertest = {
    extraGroups = [
      "docker"
      "users"
    ];
    group = "users";
    uid = 3000;
    createHome = true;
    isNormalUser = true;
  };
}
