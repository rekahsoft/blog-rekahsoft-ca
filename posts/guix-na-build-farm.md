---
title: Setup of a Simple Guix Build Farm and Substitute Server
author: Collin J. Doering
date: 2025-01-31
description: How to setup a minimal guix build farm
tags: general, guix
---

In the world of reproducible computing, GNU Guix stands out as a pioneering distribution that
enables bit-for-bit reproducible builds and a comprehensive package management system.
However, building software from source for every package can be time-consuming and
resource-intensive. This is where substitute servers play a crucial role, allowing users to
download pre-built binary packages instead of compiling them locally.

Here we built a dedicated build farm and substitute server in just few lines of code!

Guix hands us the programmer's dream of deploying a full server with its services and
orchestration as a program! Not only can the 'program' be adapted to other machines &#x2013; across
the planet &#x2013; the deployment also benefits from Guix reproducibility guarantees, including
natural roll-backs of full and partial deployment. The federated nature of these build farms
is increasingly important - in fact, I invite you to roll-your-own based on this article.

In July 2024 [I announced on the guix mailing list](https://lists.gnu.org/archive/html/guix-devel/2024-07/msg00033.html) that a new North American based Guix
substitute server and build farm, cuirass.genenetwork.org, was available for general use.
Having a server in the USA increases the speed and reduces the latency for North American
downloaders of Guix and all 20,000+ built free software packages. It also provides redundancy
in case something happens with other substitute servers.

<!--more-->

# Table of Contents

1.  [Why Build Another Substitute Server?](#orgda7cb94)
2.  [Hardware and Infrastructure](#org016c7f6)
3.  [Components of the Guix Build Farm](#org5f333aa)
    1.  [Cuirass - Building Packages](#org1076d8c)
        1.  [Define Cuirass Specs](#org940417b)
        2.  [Setup Cuirass Service](#org3c22f7a)
    2.  [Providing Substitutes using Guix Publish](#org27ccd79)
    3.  [Anonomizing IPs in logs using anonip](#org43d3c34)
    4.  [ProvidIng Web Access - Nginx Reverse Proxy](#org928dd32)
        1.  [Certbot](#orgdce4c28)
        2.  [Configure Nginx Location block for `guix-publish`](#org1f0727c)
        3.  [Nginx locations and route configuration](#org56cb33d)
        4.  [Configure Nginx Server Blocks](#orgd72d2c9)
        5.  [Nginx Service Definition](#orga8c9c85)
        6.  [Cache activation](#org859a9f5)
    5.  [Setup guix-daemon](#org2b7c0fe)
4.  [First Boot](#orgf8628df)
    1.  [Setup dhparams](#org48976f4)
    2.  [Setup Certificate Authority and Client Certificates for Cuirass Administration](#orge407cd9)
    3.  [Initial SSL/TLS Certificate/s Generation](#orgc63500f)
5.  [Installation](#orgcfaaec3)
    1.  [Remotely Bootstrapping Guix from Debian](#orgad51632)
        1.  [Guix Configuration as a Channel](#orgc51f9b4)
6.  [Challenges and Lessons Learned](#org7bcccb5)
7.  [Future Roadmap](#orgfa9e6f1)
8.  [Conclusion](#org7b26489)
    1.  [Acknowledgments](#org492af5e)

<a id="orgda7cb94"></a>


# Why Build Another Substitute Server?

The Guix ecosystem thrives on diversity and decentralization. By establishing additional
substitute servers, we achieve several critical objectives:

-   **Improved Build Diversity**: Multiple independent build farms reduce the risk of
    single-point-of-failure and increase the verification of build reproducibility.
-   **Reduced Latency**: Geographically distributed servers mean faster download times for
    users in different regions.
-   **Increased Resilience**: If one substitute server is down, users can fall back to
    alternatives.
-   **Community Contribution**: Each new substitute server strengthens the broader Guix
    infrastructure.

This article provides a comprehensive guide to setting up a Guix build farm and substitute
server, drawing inspiration from existing GNU Guix project infrastructure. You can see their
full source code [here](https://git.savannah.gnu.org/cgit/guix/maintenance.git).


<a id="org016c7f6"></a>

# Hardware and Infrastructure

The Tennessee Guix Build Farm was made possible through a collaboration with GeneNetwork.org,
who provided the following server specifications:

-   **Processor**: Dual AMD EPYC 9274F 24-Core, 48 Thread Processors
-   **RAM**: 768 GB DDR5 ECC
-   **Storage**: 1 TB SSD; 2x 3.5 TB HDD
-   **Network**: 1 Gbps nic, 100Mbps dedicated connection
-   **Location**: Memphis TN

These robust specifications allow for efficient package building, caching, and serving of
substitutes for the Guix community.

To run your own build farm that builds the entire GNU Guix distribution requires a somewhat
powerful machine. Namely, in order to keep up with upstream changes we recommend a minimum of
32GB of RAM, a 8 core CPU, and a 500GB SSD. That being said, Cuirass is flexible, so its also
possible to build a subset of the distribution and its packages, to better keep up with
package builds. Another common use-case is to use Cuirass to build custom channels and
packages (either in addition to GNU Guix, or a subset thereof).

For more details, see the [Specifications section](https://guix.gnu.org/cuirass/manual/cuirass.html#Specifications) of the Cuirass Manual.

Further measurement and analysis to more precisely determine hardware requirements for
building GNU Guix and its various packages would be valuable to the Guix community, but we
have not yet done such an analysis using our build farm.


<a id="org5f333aa"></a>

# Components of the Guix Build Farm


<a id="org1076d8c"></a>

## Cuirass - Building Packages

Cuirass is the GNU Guix continuous integration software, and is responsible for watching for
changes to one or more VCS repositories (usually [Guix channels](https://guix.gnu.org/manual/en/html_node/Channels.html)), executing build jobs for
packages that have changed, and finally, storing build results in its database.


<a id="org940417b"></a>

### Define Cuirass Specs

In order to run Cuirass via the `cuirass-service-type`, we need to define what we
want Cuirass to build. In the case of guix-na, we want to build the GNU Guix distribution and
its packages, so we declare `%cuirass-specs` as a [G-Expression](https://guix.gnu.org/manual/en/html_node/G_002dExpressions.html) that will return a list of
[cuirass specifications](https://guix.gnu.org/cuirass/manual/html_node/Specifications.html) with a single entry named "guix", which does exactly that!

```scheme
(define %cuirass-specs
  #~(list (specification
             (name "guix")
             (priority 0)
             (build '(channels guix))
             (channels %default-channels))))
```

If you have custom channels you wish to build, you would add a new `specification` to the
list. Cuirass can build more then just channels and their packages, it can also build images,
tarballs, a specific set of packages, a manifest, and more! See the [Cuirass specification](https://guix.gnu.org/cuirass/manual/html_node/Specifications.html)
documentation for more details.

Its worth noting that if you're following along by reviewing the full source code of the
Tennessee build farm that there is an additional specification listed (for guix-na itself!).
More details on that in [Guix Configuration as a Channel](#orgc51f9b4).


<a id="org3c22f7a"></a>

### Setup Cuirass Service

Now that we have defined what we want Cuirass to build, we need to specify its guix service
in the `services` field of our `operating-system` definition, which in turn will run Cuirass.
We are going to setup nginx as a reverse proxy for cuirass later on, so we'll set its host to
localhost, and pass along the specifications we defined earlier.

```scheme
(service cuirass-service-type
         (cuirass-configuration
          (host "localhost")
          (specifications %cuirass-specs)))
```

<a id="org27ccd79"></a>

## Providing Substitutes using Guix Publish

With Cuirass configured and the guix store being populated with package builds as the guix
channel changes, we now turn our attention to serving these builds as substitutes to Guix
users. This is done using [guix publish](https://guix.gnu.org/manual/en/html_node/Invoking-guix-publish.html), which Guix provides the [guix-publish-service-type](file:///home/collin/.org/roam/20221129213953-advent_of_code.md),
which is used in the `services` field of `operating-system` definition.

```scheme
(service guix-publish-service-type
         (guix-publish-configuration
          (port 3000)
          (cache "/var/cache/guix/publish")
          (ttl (* 90 24 3600))
```

Similar to Cuirass, access to guix-publish will be provided through nginx as a reverse proxy.


<a id="org43d3c34"></a>

## Anonomizing IPs in logs using anonip

Guix users care about their privacy, and though this is not necessarily a requirement,
anonymizing nginx access logs using the anonip is implemented by all public Guix sponsored
build farms, so keeping with this privacy preserving trend, cuirass.genenetwork.org
implements the same log anonymization.

To anonymize nginx access logs, the [anonip-service-type](https://guix.gnu.org/manual/devel/en/html_node/Log-Rotation.html) will be configured and used, however, we
want to anonymize multiple log files, which means multiple instances of the anonip running.
To assist with this, a helper function `anonip-service` is defined.

```scheme
(define (anonip-service file)
  (service anonip-service-type
           (anonip-configuration
            (input (format #false "/var/run/anonip/~a" file))
            (output (format #false "/var/log/anonip/~a" file)))
```

Additionally, for services that will leverage these anonymized logs (in our case, only
nginx), it will be necessary to ensure that the appropriate instance of anonip is running
prior to the respective service that will utilize it. To help declare this dependency,
another helper function is defined.

```scheme
(define (log-file->anonip-service-name file)
  "Return the name of the Anonip service handling FILE, a log file."
  (symbol-append 'anonip-/var/log/anonip/ (string->symbol file)))
```

We also define a list of anonymized log files which will be used later on along side the
`log-file->anonip-service-name` function in order to define shepherd service dependencies for
nginx.

```scheme
(define %anonip-nginx-log-files
  ;; List of files handled by Anonip for nginx
  '("http.access.log"
    "https.access.log"))
```

All that remains is to ensure that for each log file we are anonymizing, we start a
corresponding anonip-service. This can be added to the `services` field of our
`operating-system` declaration.

```scheme
(map anonip-service %anonip-nginx-log-files)
```

<a id="org928dd32"></a>

## ProvidIng Web Access - Nginx Reverse Proxy

Nginx is arguably the most complicated part of the setup. This section touches on the
essential details of configuring nginx to act as a reverse proxy for both guix-publish, and
Cuirass.


<a id="orgdce4c28"></a>

### Certbot

We would like to provide https access to cuirass, so we require a tls certificate, which we
will provision using [letsencrypt](https://letsencrypt.org/) via the [certbot](https://github.com/certbot/certbot) tool. Luckily, Guix provides a
[certbox-service-type](https://guix.gnu.org/manual/en/html_node/Certificate-Services.html) which can be used to configure certbot. As with prior services, this is
added to our `services` field in our `operating-system` configuration.

```scheme
(service certbot-service-type
         (certbot-configuration
          (email "collin@rekahsoft.ca")
          (certificates
           (list
            (certificate-configuration
             (domains '("cuirass.genenetwork.org"))
             (deploy-hook %nginx-deploy-hook))))))
```

This service references `%nginx-deploy-hook`, which we define below. It sends `SIGHUP` to
restart nginx when certbot renews certificates so the most recent certificate/s are used.

```scheme
(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))
```

Next we define a function we will use later in the [Configure Nginx Server Blocks](#orgd72d2c9) section
to lookup a certificate or private key file by host in order to reference them when
configuring Nginx tls.

```scheme
(define* (le host #:optional privkey)
  (string-append "/etc/letsencrypt/live/"
                 host "/"
                 (if privkey "privkey" "fullchain")
                 ".pem"))
```

<a id="org1f0727c"></a>

### Configure Nginx Location block for `guix-publish`

Lets define a function that given a url, produces a list of appropriate nginx location blocks
to enable guix-publish running on some provided URL.

```scheme
(define (publish-locations url)
  "Return the nginx location blocks for 'guix publish' running on URL."
  (list (nginx-location-configuration ...) ...)
```

Starting from the definition above, lets fill in and explain the purpose of each
nginx-location-configuration in the list that will be returned from our function.

1.  `/nix-cache-info`

    `guix-publish` provides a route `/nix-cache-info` which returns text/plain content of
    key/value pairs, historically used by cooperating clients and is kept for backwards
    compatibility. Given away by the reference to [Nix](https://nixos.org/) in its name, this
    route (and some aspects of the design of guix-publish) are inspired by nix-serve, and the
    [Nix Binary Cache](https://nixos.wiki/wiki/Binary_Cache).
    
    Now that we have some more context on the route, here is the nginx-location-configuration
    we will return to proxy requests appropriately.

    ```scheme
    (nginx-location-configuration
     (uri "/nix-cache-info")
     (body
      (list
       (string-append
        "proxy_pass " url "/nix-cache-info;"))))
    ```

2.  `~ \\.narinfo$`

    Normalized archives (also known as the [NAR (Nix Archive
    Format)](https://nix.dev/manual/nix/2.22/protocols/nix-archive) is used by Guix for
    cached substitutes. To get a sense for how substitutes are downloaded from a guix-publish
    substitute server, lets take a moment to do so manually for the
    [hello](https://packages.guix.gnu.org/packages/hello/2.12.1/) package.
    
    First, lets find the store path of the package (but without actually building it).
    
    ``` {.bash .code-term}
    $ guix build --dry-run hello
    /gnu/store/8bjy9g0cssjrw9ljz2r8ww1sma95isfj-hello-2.12.1
    ```

    The result is composed of a few parts:
    
    -   **`/gnu/store/`:** the guix store path
    -   **`8bjy9g0cssjrw9ljz2r8ww1sma95isfj`:** a hash uniquely identifying the store item
    -   **`-hello-2.12.1`:** the package-name and version, separated by dashes
    
    We now have enough context to define our route matching anything that ends in `.narinfo`.
    
    ```scheme
    (nginx-location-configuration
     (uri "~ \\.narinfo$")
     (body
      (list
       ;; Since 'guix publish' has its own caching, and since it relies
       ;; on the atime of cached narinfos to determine whether a
       ;; narinfo can be removed from the cache, don't do any caching
       ;; here.
       (string-append "proxy_pass " url ";"))))
   ```

3.  `/nar/`

    As part of defining the nginx-location-configuration for `*.narinfo` routes, we started to
    manually fetch a substitute. Here we will continue, using the provided hash to query to
    query the substitute server for a corresponding `.narinfo` file.
    
    ``` {.bash .code-term}
    $ curl https://cuirass.genenetwork.org/8bjy9g0cssjrw9ljz2r8ww1sma95isfj.narinfo
    StorePath: /gnu/store/8bjy9g0cssjrw9ljz2r8ww1sma95isfj-hello-2.12.1
    NarHash: sha256:0f94l0bl09i2igkhklzkawqbbdn4kkxl90wbb4y7f0dnni4f6ljh
    NarSize: 235240
    References: 8bjy9g0cssjrw9ljz2r8ww1sma95isfj-hello-2.12.1 zvlp3n8iwa1svxmwv4q22pv1pb1c9pjq-glibc-2.39 zzpbp6rr43smwxzvzd4qd317z5j7qblj-gcc-11.4.0-lib
    Deriver: 79dhya6sngg4rf53m1cyxlhn8y4pnw2n-hello-2.12.1.drv
    Signature: 1;balg02;KHNpZ25hdHVyZSAKIChkYXRhIAogIChmbGFncyByZmM2OTc5KQogIChoYXNoIHNoYTI1NiAjNjE0ODFDNDUzMkU3RTIyOUEzMDlDREVBRDM2MkE2Qzk4QjU0RkFDNEUyQjA1ODEzQ0ZDOEI1NzQ2RUY0NjYxMiMpCiAgKQogKHNpZy12YWwgCiAgKGVjZHNhIAogICAociAjMDQ2NTA3Q0FBNUJFNEY1QUQxRUE0NzUwQzlEMjgzMjQ5NDMwMDQ1OEIzRTM5QUJDOTBFMjZGNkU0MTA0RjMwNCMpCiAgIChzICMwQjdERDlCRUE5ODA0MTkyQ0E2OTUwQzFGRUYzRDdEQ0M3RTMyQzNEMENGNDg3NkY4RkRBMzEwRTUzNkYwNEVBIykKICAgKQogICkKIChwdWJsaWMta2V5IAogIChlY2MgCiAgIChjdXJ2ZSBFZDI1NTE5KQogICAocSAjOTU3OEFENkNEQjIzQkE1MUY5QzQxODVENUQ1QTMyQTdFRUI0N0FDREQ1NUYxQ0NCOENFRTRFMDU3MEZCRjk2MSMpCiAgICkKICApCiApCg==
    URL: nar/gzip/8bjy9g0cssjrw9ljz2r8ww1sma95isfj-hello-2.12.1
    Compression: gzip
    FileSize: 73331
    URL: nar/zstd/8bjy9g0cssjrw9ljz2r8ww1sma95isfj-hello-2.12.1
    Compression: zstd
    FileSize: 65480
    ```
    
    If the package is not available, this would return a `404` not found error. However, in our
    case the substitute is found, and various details about it are provided back to the
    requester. The `URL` field is notable, as it will be used next to download the substitute,
    which is why we define another proxied route for `/nar/` below.
    
    ```scheme
    (nginx-location-configuration
     (uri "/nar/")
     (body
      (list
       (string-append "proxy_pass " url ";"))))
    ```

4.  `/robots.txt`

    First, lets define a string `publish-robots.txt`, that we'll configure Nginx to serve on the
    `/robots.txt` route to prevent good-faith crawlers from downloading substitutes.
    
    ```scheme
    (define publish-robots.txt
      ;; Try to prevent good-faith crawlers from downloading substitutes.  Allow
      ;; indexing the root—which is expected to be static or cheap—to remain visible
      ;; in search engine results for, e.g., 'Guix CI'.
      "\
    User-agent: *\r
    Disallow: /\r
    Allow: /$\r
    \r
    ")
    ```
    
    To serve this `robots.txt` we use a g-exp to store its contents as a file in the guix
    store, to be served by Nginx on the `/robots.txt` route.
    
    ```scheme
    ;; Try to prevent good-faith crawlers from downloading substitutes.
    (nginx-location-configuration
     (uri "= /robots.txt")
     (body
      (list
       #~(string-append "try_files "
                        #$(plain-file "robots.txt" publish-robots.txt)
                        " =404;")
       "root /;")))
   ```

<a id="org56cb33d"></a>

### Nginx locations and route configuration

Lets define another function that will be used to produce the
`nginx-location-configurations`'s we will use in our `operating-system` configuration,
including setup for guix-publish (using the previously defined `publish-locations`
function above) in addition to setup for Cuirass and Certbot.

```scheme
(define (balg02-locations publish-url)
  "Return nginx location blocks with 'guix publish' reachable at PUBLISH-URL."
  (append (publish-locations publish-url)
          (list ...)))
```

Similar to beforehand, starting from the definition above, lets fill in and explain the
purpose of each nginx-location-configuration in the list that will be returned from our
function.

1.  `/`

    Cuirass and its routes should be accessible, which exactly what the below
    nginx-configuration does! It proxies traffic to the `cuirass` service (which is
    assumed to be running at its default location).
    
    ```scheme
    (nginx-location-configuration
     (uri "/")
     (body (list "proxy_pass http://localhost:8081;")))
    ```

2.  `/admin`

    As stated in the [Cuirass manual](https://guix.gnu.org/cuirass/manual/cuirass.html#Authentication), "Cuirass does not provide its own authentication
    mechanism; by default, any user can do anything via its web interface. To restrict this to
    only authorized users, one approach is to proxy the Cuirass web site via a web server such
    as Nginx and configure the web server to require client certificate verification for pages
    under the ‘/admin’ prefix."
    
    Here we do exactly as recommended by the manual: restrict access to `/admin` Cuirass routes
    by checking ssl client certificate validity, proxying to cuirass when successful, and
    returning 403 Unauthorized otherwise.
    
    ```scheme
    (nginx-location-configuration
     (uri "~ ^/admin")
     (body
      (list "if ($ssl_client_verify != SUCCESS) { return 403; } proxy_pass http://localhost:8081;")))
    ```

3.  `/static`

    Cuirass serves a series of static js/css/media files for its web interface. These are
    available via the `/static` route, which we add a `nginx-location-configuration` for below.
    
    ```scheme
    (nginx-location-configuration
     (uri "/static")
     (body
      (list
       "proxy_pass http://localhost:8081;"
       ;; Cuirass adds a 'Cache-Control' header, honor it.
       "proxy_cache static;"
       "proxy_cache_valid 200 2d;"
       "proxy_cache_valid any 10m;"
       "proxy_ignore_client_abort on;")))
    ```

4.  `/download`

    Cuirass allows users to download build products (eg. system images). To enable this we must
    proxy `/download` routes to cuirass.
    
    ```scheme
    (nginx-location-configuration
     (uri "/download")                     ;Cuirass "build products"
     (body
      (list
       "proxy_pass http://localhost:8081;"
       "expires 10d;"                      ;override 'Cache-Control'
       "proxy_cache static;"
       "proxy_cache_valid 200 30d;"
       "proxy_cache_valid any 10m;"
       "proxy_ignore_client_abort on;")))
    ```
    
    This concludes the configuration of routes for Cuirass.

5.  `/.well-known`

    Now we configure the `/.well-known` route, to enable Certbot certificate rotation. Namely,
    Certbot uses the `/.well-known` route to serve its challenge/response files.
    
    ```scheme
    (nginx-location-configuration          ;certbot
     (uri "/.well-known")
     (body (list "root /var/www;")))
    ```
    
    For more details, see the ["HTTP-01 challenge" section of the Lets Encrypt Challenge Types](https://letsencrypt.org/docs/challenge-types/)
    documentation. For Guix specific details, check out the [certbot-service-type](https://guix.gnu.org/manual/en/html_node/Certificate-Services.html) documentation.


<a id="orgd72d2c9"></a>

### Configure Nginx Server Blocks

Now that we have a function (`balg02-locations`) that given a guix-publish url to proxy, will
output a list of `nginx-location-configuration`'s for Cuirass, guix-publish, and certbot, we
set our attention on setting up the necessary `nginx-server-configuration`'s for our build
farm.

First, we define a few constants that will be reused across the http and https server
configurations later on.

```scheme
(define %publish-url "http://localhost:3000")

(define %tls-settings
  (list
   ;; Make sure SSL is disabled.
   "ssl_protocols       TLSv1.1 TLSv1.2 TLSv1.3;"
   ;; Disable weak cipher suites.
   "ssl_ciphers         HIGH:!aNULL:!MD5;"
   "ssl_prefer_server_ciphers on;"

   ;; Use our own DH parameters created with:
   ;;    openssl dhparam -out dhparams.pem 2048
   ;; as suggested at <https://weakdh.org/sysadmin.html>.
   "ssl_dhparam         /etc/dhparams.pem;"))
```

Next, we'll define our `nginx-server-configuration`'s, starting with the following definition
which we will fill in one `nginx-server-configuration` at a time.

```scheme
(define %balg02-servers
  (list ...))
```

Our first `nginx-server-configuration` in our list is forward-looking: it puts in place a
http to https redirect for sites we may add in the future.

```scheme
;; Redirect domains that don't explicitly support HTTP (below) to HTTPS.
(nginx-server-configuration
 (listen '("80"))
 (raw-content
  (list "return 308 https://$host$request_uri;")))
```

Our second, configures nginx to listen for HTTP traffic on its default port for our domain.
Here we leverage the `balg02-locations` function we defined earlier, and make sure to send
our log files through anonip which we previously configured.

```scheme
;; Domains that still explicitly support plain HTTP.
(nginx-server-configuration
 (listen '("80"))
 (server-name `("cuirass.genenetwork.org"
                ;; <https://logs.guix.gnu.org/guix/2021-11-20.log#155427>
                "~[0-9]$"))
 (locations (balg02-locations %publish-url))
 (raw-content
  (list
   "access_log  /var/run/anonip/http.access.log;"
   "proxy_set_header X-Forwarded-Host $host;"
   "proxy_set_header X-Forwarded-Port $server_port;"
   "proxy_set_header X-Forwarded-For  $proxy_add_x_forwarded_for;")))
```

Our last `nginx-server-configuration` configures nginx to listen for HTTPS traffic on its
default port (443); it is nearly identical to the previous `nginx-server-configuration`, but
makes use of the `le` lets encrypt helper function we defined earlier to lookup the
appropriate ssh certificate or key.

Additionally, it specifies a SSL/TLS CA certificate to use for client certificate
verification. Later on in [Setup Certificate Authority and Client Certificates for Cuirass
Administration](#orge407cd9), we'll provide some details on how the CA and client certificates can be
generated.

```scheme
;; HTTPS servers
(nginx-server-configuration
 (listen '("443 ssl"))
 (server-name '("cuirass.genenetwork.org"))
 (ssl-certificate (le "cuirass.genenetwork.org"))
 (ssl-certificate-key (le "cuirass.genenetwork.org" 'key))
 (locations (balg02-locations %publish-url))
 (raw-content
  (append
   %tls-settings
   (list
    "access_log  /var/run/anonip/https.access.log;"
    "proxy_set_header X-Forwarded-Host $host;"
    "proxy_set_header X-Forwarded-Port $server_port;"
    "proxy_set_header X-Forwarded-For  $proxy_add_x_forwarded_for;"
    ;; For Cuirass admin interface authentication
    "ssl_client_certificate /etc/ssl-ca/certs/ca.crt;"
    "ssl_verify_client optional;"))))
```

<a id="orga8c9c85"></a>

### Nginx Service Definition

We are nearing the conclusion of configuring nginx! Last up we will define some extra content
which will be included with our nginx configuration.

```scheme
(define %extra-content
  (list
   "default_type  application/octet-stream;"
   "sendfile        on;"

   ;; Maximum chunk size to send.  Partly this is a workaround for
   ;; <http://bugs.gnu.org/19939>, but also the nginx docs mention that
   ;; "Without the limit, one fast connection may seize the worker
   ;; process entirely."
   ;;  <http://nginx.org/en/docs/http/ngx_http_core_module#sendfile_max_chunk>
   "sendfile_max_chunk 1m;"

   "keepalive_timeout  65;"

   ;; Use HTTP 1.1 to talk to the backend so we benefit from keep-alive
   ;; connections and chunked transfer encoding.  The latter allows us to
   ;; make sure we do not cache partial downloads.
   "proxy_http_version 1.1;"

   ;; The 'inactive' parameter for caching is not very useful in our
   ;; case: all that matters is that LRU sweeping happens when 'max_size'
   ;; is hit.

   ;; cache for build logs
   "proxy_cache_path /var/cache/nginx/logs"
   "     levels=2"
   "     inactive=60d"          ; inactive keys removed after 60d
   "     keys_zone=logs:8m"     ; narinfo meta data: ~64K keys
   "     max_size=4g;"          ; total cache data size max

   ;; cache for static data
   "proxy_cache_path /var/cache/nginx/static"
   "     levels=1"
   "     inactive=10d"	       ; inactive keys removed after 10d
   "     keys_zone=static:1m"   ; nar cache meta data: ~8K keys
   "     max_size=200m;"        ; total cache data size max

   ;; If Hydra cannot honor these delays, then something is wrong and
   ;; we'd better drop the connection and return 504.
   "proxy_connect_timeout 10s;"
   "proxy_read_timeout 10s;"
   "proxy_send_timeout 10s;"

   ;; Cache timeouts for a little while to avoid increasing pressure.
   "proxy_cache_valid 504 30s;"))
```

Finally, all of our hard work comes together with a simple `nginx-configuration`.

```scheme
(define %nginx-configuration
  (nginx-configuration
   (server-blocks %balg02-servers)
   (server-names-hash-bucket-size 128)
   (global-directives
    '((worker_processes . 16)
      (pcre_jit . on)
      (events . ((worker_connections . 1024)))))
   (extra-content
    (string-join %extra-content "\n"))
   (shepherd-requirement
    (map log-file->anonip-service-name
         %anonip-nginx-log-files))))
```

<a id="org859a9f5"></a>

### Cache activation

To ensure the nginx cache folder exists on the file-system prior to the first run of nginx, we
create a [simple-service](https://guix.gnu.org/manual/en/html_node/Service-Reference.html) that creates the `/var/cache/nginx` folder upon system activation.

```scheme
(define %nginx-cache-activation
  ;; Make sure /var/cache/nginx exists on the first run.
  (simple-service 'nginx-/var/cache/nginx
                  activation-service-type
                  (with-imported-modules '((guix build utils))
                    #~(begin
                        (use-modules (guix build utils))
                        (mkdir-p "/var/cache/nginx")))))
```

This service can then be added to the `services` field of our `operating-system`
configuration, finalization our configuration of nginx.


<a id="org2b7c0fe"></a>

## Setup guix-daemon

Lets start by defining a function that given a few configuration options we'll detail in a
moment, returns a `guix-configuration` that can be used when creating a `guix-service-type`
for our `operating-system`.

-   **`max-jobs`:** an integer denoting how many guix-daemon build jobs can run in parallel
-   **`cores`:** an integer denoting how many CPU cores guix-daemon builds can use
-   **`authorized-keys`:** a list of file-like objects denoting guix signing keys
-   **`substitute-urls`:** a list of strings denoting substitute server urls

```scheme
(define* (guix-daemon-config #:key (max-jobs 5) (cores 4)
                             (build-accounts-to-max-jobs-ratio 4)
                             (authorized-keys '())
                             (substitute-urls '()))
  (guix-configuration
   (substitute-urls substitute-urls)
   (authorized-keys authorized-keys)

   ;; We don't want to let builds get stuck for too long, but we still want
   ;; to allow building things that can take a while (eg. 3h). Adjust as necessary.
   (max-silent-time 3600)
   (timeout (* 6 3600))

   (log-compression 'gzip)               ;be friendly to 'guix publish' users

   (build-accounts (* build-accounts-to-max-jobs-ratio max-jobs))
   (extra-options (list "--max-jobs" (number->string max-jobs)
                        "--cores" (number->string cores)
                        "--gc-keep-derivations"))))
```

We can now use our `guix-daemon-config` function to adjust the `guix-service-type`
configuration in our `operating-system`. Notably, we allow for substitutes from this server
(by providing its substitute-url and its authorized-key) and set other options to reasonable
defaults.

```scheme
(modify-services %base-services
                 (guix-service-type config => (guix-daemon-config
                                               #:substitute-urls
                                               '("https://cuirass.genenetwork.org")
                                               #:max-jobs 20
                                               #:cores 4
                                               #:authorized-keys
                                               (cons
                                                (local-file "../../../.pubkeys/guix/cuirass.genenetwork.org.pub")
                                                %default-authorized-guix-keys)
                                               #:build-accounts-to-max-jobs-ratio 5)))
```

<a id="orgf8628df"></a>

# First Boot

There are some opportunities to improve our deployment, as currently it requires manual
intervention after the initial installation. Here we'll detail whats necessary.


<a id="org48976f4"></a>

## Setup dhparams

Earlier in [Configure Nginx Server Blocks](#orgd72d2c9), we specified our own DH parameters, as suggested by
<https://weakdh.org/sysadmin.html>. We need to create them after the first boot of our build farm.

``` {.bash .code-term}
$ openssl dhparam -out /etc/dhparams.pem 2048
```

<a id="orge407cd9"></a>

## Setup Certificate Authority and Client Certificates for Cuirass Administration

Setup of SSL/TLS client side certificates is beyond the scope of this article, however we
will briefly mention and demonstrate how the [etc/new-client-cert.scm](https://git.savannah.gnu.org/cgit/guix/guix-cuirass.git/tree/etc/new-client-cert.scm) script that is part of
the Cuirass repository (but not in the provided cuirass guix package) can be adjusted to fit
the needs of most Cuirass deployments.

First fetch the script and make it executable.

``` {.bash .code-term}
$ curl -O https://git.savannah.gnu.org/cgit/guix/guix-cuirass.git/tree/etc/new-client-cert.scm
$ chmod +x new-client-cert.scm
```

This script leverages `guix` itself to be run, so its necessary to be running on a system
with guix installed in order to use this script. For more details on how this is done, see
the note on the [Invoking guix shell](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-shell.html) documentation about "guix shell can also be used as a
script interpreter".

The script currently needs to be slightly adjusted to alter the `subject-template` being
used, which by default is `"/C=DE/ST=Berlin/L=Berlin/O=GNU Guix/OU=Cuirass/CN=~a"` - this
string defines the openssl subject name used when generating a CA and client certificates,
where `~a` will be substituted for a name later on. Here you may want to adjust the location,
organization, etc.. to suite your case.

First a Certificate Authority needs to be generated. This should be done from the cuirass
server itself so the CA key never leaves the server its being used on, and can be done with
the `new-client-cert.scm` script as follows.

``` {.bash .code-term}
$ ./new-client-cert.scm --generate-ca
```

By default, this generates a CA key `/etc/ssl-ca/private/ca.key` and certificate
`/etc/ssl-ca/certs/ca.crt`, so is important to ensure these directories already exist and are
writable by the user running the script.

``` {.bash .code-term}
$ mkdir -p /etc/ssl-ca/{private,cert}
```

With the CA in place, one or more client certificates can now be generated. First, clients
will need to supply a Certificate Signing Request (CSR), which should be placed in the
current working directory and named `<who>.csr` where `<who>` should be the name of the
certificate signing request (eg. `collin.csr`). We can then run the following to generate a
signed client certificate, which will be outputted as `<who>.p12` in the current working
directory.

``` {.bash .code-term}
$ ./new-client-cert.scm collin
```

The `collin.p12` [PKCS 12](https://en.wikipedia.org/wiki/PKCS_12) formatted certificate file that was generated by the above command
can now be provided back to the requester, to be installed in their browser or operating
system, enabling administrative access to cuirass.


<a id="orgc63500f"></a>

## Initial SSL/TLS Certificate/s Generation

In order for the `nginx-service` to run successfully, it needs SSL/TLS certificates, which
upon initial boot will not be available. This causes a cascade of service failures until the
initial certificates are generated. However, there is a chicken-and-egg problem - the nginx
service needs to be running to provide http access to the `/.well-known` route so certbot can
complete. This can be worked around by providing a self-signed certificate and key
temporarily for each certificate/key refereed to, which in our case is only one.

``` {.bash .code-term}
$ mkdir -p /etc/letsencrypt/live/cuirass.genenetwork.org
$ openssl req -x509 -nodes -days 1 -newkey rsa:2048 -keyout /etc/letsencrypt/live/cuirass.genenetwork.org/privkey.pem -out /etc/letsencrypt/live/cuirass.genenetwork.org/fullchain.pem -subj "/CN=temporary"
```

Following creating a temporary self-signed certificate, we can start nginx.

``` {.bash .code-term}
$ sudo herd start nginx
```

And now, we can force certbot to renew our certificates.

``` {.bash .code-term}
$ sudo herd start renew-certbot-certificates
```

Once this completes, nginx will automatically be restarted (using the `%nginx-deploy-hook` we
setup earlier when setting up [Certbot](#orgdce4c28)), and will use the new Let's Encrypt certificates.

If the cuirass service is still in a failing state, it can now be restarted.


<a id="orgcfaaec3"></a>

# Installation

We've left out other aspects of defining our `operating-system` configuration, as its [well
documented](https://guix.gnu.org/manual/devel/en/html_node/operating_002dsystem-Reference.html) by the Guix project, and varies depending on the specifics of your machine (for
instance, file-systems, users/groups, initrd-modules, etc..). Once assembled, the
`operating-system` configuration can be placed in a file `system.scm`, which defines a guix
system configured with Cuirass, guix-publish, guix-daemon, and nginx, setup to act as a
single-node build farm and substitute server! It can be trivially installed following the
[manual Guix System installation documentation](https://guix.gnu.org/manual/en/html_node/Proceeding-with-the-Installation.html).

``` {.bash .code-term}
$ guix system init system.scm /mnt
```

Here `/mnt` is where the target root file-system is mounted after being prepared for
installation. See our [Initial Setup Documentation](https://git.genenetwork.org/guix-north-america/tree/docs/initial-setup.org) for the specifics of preparing the disks
for Guix installation in our case.


<a id="orgad51632"></a>

## Remotely Bootstrapping Guix from Debian

In our case with the Tennessee Build Farm, physical access was inconvenient due to travel
distance, so installation needed to be completed remotely. The target server already had
Debian running on it, configured with serial access available out-of-band via [Dell iDRAC](https://en.wikipedia.org/wiki/Dell_DRAC) as
well as ssh access to Debian. Another ssd was available for use, and was the target of our
Guix installation.

Guix can be [installed on foreign distributions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html), which is well documented, so its not covered
here, but is the first step in bootstrapping Guix from Debian.


<a id="orgc51f9b4"></a>

### Guix Configuration as a Channel

Once Guix (the package manager) is installed on Debian, we need to partition our drives as
required (which again varies, so will not be covered here). Next we need to make our
`operating-system` configuration available so we can complete bootstrapping Guix. The most
straightforward way to do so is to just copy the `system.scm` file we defined earlier.
Another way is to capture the configuration in a git repository, and [make it a Guix channel](https://guix.gnu.org/manual/en/html_node/Creating-a-Channel.html).
This enables great shared tracking of system changes over time (outside of [guix system
list-generations &#x2026;](https://guix.gnu.org/manual/en/html_node/Invoking-guix-system.html)), and is what was done while bootstrapping our installation.

We choose to include a `channels.scm` file as part of the configuration channel that pins the
versions of software that will be used with our deployment. So, in order to boot the server
using an `operating-system` defined in [our configuration channel](https://git.genenetwork.org/guix-north-america/), we first download the
`channels.scm` file.

``` {.bash .code-term}
$ curl -O https://git.genenetwork.org/guix-north-america/plain/channels.scm
```

We then create a temporary `bootstrap.scm` file that contains a references to the
`operating-system` we defined in our configuration channel.

```scheme
(@ (guix-na config balg02) balg02)
```

We then use `guix time-machine` to specify these channels when installing Guix onto the
system.

``` {.bash .code-term}
$ guix time-machine -C channels.scm -- system init bootstrap.scm /mnt
```

Subsequent updates to the system can be done without using the `bootstrap.scm` file. For
instance, say the guix channel is updated in `channels.scm`. To apply this change to the
server, the new channels would need to be pulled, and the system reconfigured.

``` {.bash .code-term}
$ sudo -i guix pull -C <(curl https://git.genenetwork.org/guix-north-america/plain/channels.scm)
$ sudo -i guix system reconfigure -e '(@ (guix-na config balg02) balg02)'
```

One caveat to using `-e|--expression` is that currently this expression is not stored along
side the guix system generation, which makes it not possible to know which `operating-system`
configuration was used from a given channel (reported upstream as issue [#54631](https://issues.guix.gnu.org/54631)). To work
around this for the time being, a file containing this expression can be used (just like was
used for bootstrapping).


<a id="org7bcccb5"></a>

# Challenges and Lessons Learned

Setting up a public Guix substitute server is not without its challenges:

1.  **Performance Tuning**: Configuring Cuirass and the Guix daemon to efficiently use
    available resources required careful optimization.
2.  **Privacy Considerations**: Implementing IP anonymization with anonip was crucial to
    protect user privacy.
3.  **Bandwidth and Storage Management**: Implementing intelligent caching strategies to
    manage storage and network resources.

Luckily, many of these challenges had already been sorted out by existing Guix build farms,
making this endeavor much easier.

The biggest challenge was remote installation, where one hiccup was not realizing the
`megaraid_sas` module was needed for our root ssd used for Guix, resulting in a failed first
boot following bootstrapping. Luckily this was resolved by booting to Debian and
reconfigure-ing Guix after adjusting our `initrd-modules` to include `megaraid_sas`.


<a id="orgfa9e6f1"></a>

# Future Roadmap

Looking ahead, we have several goals for the cuirass.genenetwork.org substitute server:

-   Collaborate with Guix maintainers to potentially include this server in the included list
    of default Guix substitute servers
-   Expand build coverage to include more architectures and specialized packages
-   Implement more sophisticated monitoring and performance tracking
-   Explore potential partnerships with other academic and research institutions


<a id="org7b26489"></a>

# Conclusion

The Tennessee Guix Build Farm represents more than just a technical infrastructure project.
It embodies the spirit of open-source collaboration, community-driven development, and the
principles of reproducible computing. By providing a robust, privacy-conscious substitute
server, we hope to contribute to the growth and accessibility of the GNU Guix ecosystem.

We invite other organizations, universities, and community members to consider setting up
their own substitute servers. Each new node makes the Guix network stronger, more resilient,
and more accessible.


<a id="org492af5e"></a>

## Acknowledgments

We thank the Guix North America team consisting of Collin Doering, Pjotr Prins and Arun
Isaac.

We wish to acknowledge the GNU Guix project, in particular Ludovic Courtès, Efraim Flashner,
Christopher Baines, Ricardo Wurmus, Christine Lemmer-Webber and Andy Wingo for their
incredible work on GNU Guix and GNU Guile and are a continuous inspiration. Without their
input our work would not be possible. We also wish to acknowledge the fantastic [GeneNetwork
and Pangenome teams](<https://genenetwork.org/facilities/>) for providing support and hardware,
including the spiffy AMD Genoa machine that runs the Tennessee Guix Build Farm at
Memphis-based [Worldspice](<https://www.worldspice.net/>).

