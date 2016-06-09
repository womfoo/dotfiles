with import <nixpkgs> {}; {
  rubyEnv = stdenv.mkDerivation {
    name = "puppet-env";
    buildInputs = [
      ruby_2_1
      facter
    ];
    shellHook = ''
      export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
      export RUBYLIB=${facter}/lib/ruby
      export GEM_PATH=/home/kranium/.gem/ruby/2.1.0
      export PATH=$PATH:~/.gem/ruby/2.1.0/bin
    '';
  };
}
