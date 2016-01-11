/**
 * (C) Copyright Collin Doering 2013
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * File:. default.js
 * Author: Collin J. Doering
 * Date: Sep  6, 2013
 * Description: Client-side logic for blog-rekahsoft-ca
 */

//------------------------
/*jslint browser: true*/
/*global jQuery, MathJax*/
//------------------------

// Global array for processing piwik analytics commands
var _paq = _paq || [];

(function ($, mj) {
    "use strict";

    var router = (function () {
        var routes = [],
            routeDefault = { acceptUrls: /.*/ },
            initCallbacks = [],
            onChangeCallbacks = [],
            beforeSends = [],
            onSuccesss = [],
            onErrors = [],

            spec = {
                runRouter: function runRouter(url) {
                    var i;
                    for (i = 0; i < routes.length; i += 1) {
                        if (routes[i].acceptUrls.test(url)) {
                            load(url, routes[i]);
                            break;
                        }
                    }

                    // No route ran; run default route
                    if (i === routes.length) {
                        load(url, routeDefault);
                    }
                },
                onInit: function onInit(cb) {
                    if (typeof cb === "function") {
                        initCallbacks.push(cb);
                    }
                },
                onChange: function onChange(cb) {
                    if (typeof cb === 'function') {
                        onChangeCallbacks.push(cb);
                    }
                },
                beforeSend: function beforeSend(cb) {
                    if (typeof cb === "function") {
                        beforeSends.push(cb);
                    }
                },
                onSuccess: function onSuccess(cb) {
                    if (typeof cb === "function") {
                        onSuccesss.push(cb);
                    }
                },
                onError: function onError(cb) {
                    if (typeof cb === "function") {
                        onErrors.push(cb);
                    }
                },
                route: function route(r) {
                    if (typeof r === "object" && r.acceptUrls instanceof RegExp) {
                        routes.push(r);
                    } else {
                        throw "Error invalid route";
                    }
                },
                defaultRoute: function defaultRoute(r) {
                    if (typeof r === "object") {
                        r.acceptUrls = /.*/;
                        routeDefault = r;
                    }
                },
                jsUrls: function (sel) {
                    $(sel).each(function (i) {
                        var href = $(this).attr('href'),
                            external_url_regexp = /https?:\/\/.*/,
                            mailto_regexp = /mailto:.*/,
                            files_regexp = /files\/.*/,
                            images_regexp = /images\/.*/;

                        if (!(external_url_regexp.test(href) || mailto_regexp.test(href) || files_regexp.test(href) || images_regexp.test(href))) {
                            $(this).click(function (e) {
                                e.preventDefault();
                                spec.runRouter(href);
                            });
                        }
                    });
                }
            };

        function setUrl(url) {
            if (url === "/index.html") {
                history.pushState(null, "Home", "/");
            } else {
                // TODO: strip url into title
                history.pushState(null, "Title", url);
            }
            runCallbacks(url);
        }

        function load(url, r) {
            // Set url in navigation bar
            setUrl(url);

            // Load route using ajax
            $.ajax({
                url: url,
                type: 'GET',
                dataType: 'html',
                beforeSend: function (xhr, settings) {
                    // Run router beforeSend handlers
                    beforeSends.forEach(function (cb) {
                        cb(url);
                    });

                    // Run current handlers onSuccess callback (if it exists)
                    if (r.hasOwnProperty('beforeSend') && typeof r.beforeSend === 'function') {
                        r.beforeSend(url);
                    }
                },
                success: function (dta) {
                    // Parse script tags out of html
                    dta = $($.parseHTML(dta)).filter('#page-content').html();

                    // Add anchor click handlers for internal links in new content
                    spec.jsUrls('#page-content a');

                    // Run router onSuccess handlers
                    onSuccesss.forEach(function (cb) {
                        cb(url, dta);
                    });

                    // Run current handles onSuccess callback (if it exists)
                    if (r.hasOwnProperty('onSuccess') && typeof r.onSuccess === 'function') {
                        r.onSuccess(url, dta);
                    }
                },
                error: function (xhr, status) {
                    // Run router onError handlers
                    onErrors.forEach(function (cb) {
                        cb(url, status);
                    });

                    // Run current handles onError callback (if it exists)
                    if (r.hasOwnProperty('onError') && typeof r.onError === 'function') {
                        r.onError(url, status);
                    }
                }
            });
        }

        function runCallbacks(url) {
            // Run each of the onChangeCallbacks
            onChangeCallbacks.forEach(function (cb) {
                cb(url);
            });
        }

        // Listen for load event and run initCallbacks
        window.addEventListener("load", function () {
            initCallbacks.forEach(function (cb) {
                cb(location.pathname);
            });
        });

        // Listen for popstate event and call router with the new url
        window.addEventListener("popstate", function (e) {
            router.runRouter(location.pathname);
        });

        return spec;
    }()),

    analytics = (function () {
        var inited = false,
            spec = {
                trackPageView: trackPageView,
                debugEnable: function () {
                    init();
                }
            };

        function trackPageView (href) {
            if (inited) {
                _paq.push(["setDocumentTitle", document.domain + href]);
                _paq.push(["trackPageView"]);
            }
        }

        function init() {
            if (!inited) {
                _paq.push(["setDoNotTrack", true]);
                _paq.push(["enableLinkTracking"]);
                _paq.push(["setTrackerUrl", "//analytics.rekahsoft.ca/piwik.php"]);
                _paq.push(["setSiteId", 1]);

                inited = true;
            }
        }

        // Initialize piwik.js when site is initially loaded
        router.onInit(function () {
            if (document.domain != "localhost") {
                init();
                trackPageView('/');
            }
        });

        // Track page views with piwik each time the url changes
        router.onChange(function (url, dta) {
            trackPageView(url);
        });

        return spec
    }()),

    site = (function () {
        var spec = {
            init: init
        },

        status = (function () {
            var messages = [],
                validIndicators = ["error", "success", "info"],
                spec = {
                    setMessage: function setMessage(indicator, msg) {
                        var message = {}, hasValidIndicator = false;
                        for (var i = 0; i < validIndicators.length; i++) {
                            if (indicator === validIndicators[i]) {
                                hasValidIndicator = true;
                                break;
                            }
                        }

                        if (!hasValidIndicator) {
                            throw "The indicator given '" + indicator + "' is not know";
                        } else {
                            message.indicator = indicator;
                            message.msg = msg;
                            messages.push(message);
                            // TODO: Set the current message and appropriate class on #status given the indicator
                            // parameter, which can be one of "error", "success", or "info".
                        }
                    },
                    hasMessage: function hasMessage() {
                        if (messages.length === 0) {
                            return false;
                        } else {
                            return true;
                        }
                    },
                    validIndicators: validIndicators,
                    clear: function clear() {
                        // clear the status messages
                        messages = [];
                        $('#status').html('');
                    },
                    hide: function hide(indication) {
                        $('#status').slideUp('normal', function () {
                            $('#status').removeClass('error').removeClass('success').children('p.message').remove();
                        });
                    }
                };

            router.onSuccess(function (url, dta) {
                status.hide();
            });

            $(document).ready(function () {
                $('#status a.close-button').click(function () {
                    $(this).parent().slideUp(function () {
                        $(this).removeClass('error').removeClass('success');
                        $(this).children('p.message').remove();
                    });
                });
            });

            return spec;
        }()),

        nav = (function () {
            var spec = {
                setActive: function setActive(url) {
                    if (url === "/") {
                        url = "/index.html";
                    }

                    // Initially set the active menuitem in the nav
                    $('a.menuitem[href="' + url + '"]').closest('ul').find('li.active').removeClass('active');
                    $('a.menuitem[href="' + url + '"]').closest('li').addClass('active');
                },
                toggleLoading: function toggleLoading() {
                    $('#nav').toggleClass('loading');
                },
                loadingOff: function loadingOff() {
                    $('#nav').removeClass('loading');
                },
                loadingOn: function loadingOn() {
                    $('#nav').addClass('loading');
                }
            };

            router.onChange(function (url, dta) {
                nav.setActive(url);
            });

            router.beforeSend(function (url) {
                nav.toggleLoading();
            });

            $(document).ready(function () {
                $('#nav-menu a.menuitem').click(function () {
                    $(this).closest('ul').find('li.active').removeClass('active');
                    $(this).closest('li').addClass('active');
                });
            });

            return spec;
        }());

        function appCacheUpdateReady () {
            window.applicationCache.swapCache();
            // TODO: find what resource is loaded currently and reload it if it has changed
        }

        function init() {
            window.addEventListener("updateready", appCacheUpdateReady);
            if (window.applicationCache.status === window.applicationCache.UPDATEREADY) {
                appCacheUpdateReady();
            }

            // TODO: deal with jsUrls function which is moved to the router
            $(document).ready(function () {
               // Add anchor click handlers for internal links
               router.jsUrls('#page-content a, #nav-menu a.menuitem');
            });
        }

        return spec;
    }());

    // Initialize routes
    router.route({ // Post pages handler
        acceptUrls: /posts\/.*\.html/
    });

    router.route({ // Tag pages handler
        acceptUrls: /tags\/.*(\d*)\.html/
    });

    router.route({ // Blog pages handler
        acceptUrls: /blog\d*\.html/
    });

    router.defaultRoute({ // Default page handler
        beforeSend: undefined,
        onSuccess: undefined,
        onError: undefined
    });

    router.beforeSend(function (url) {
        // Add .loading to #page-content and #nav to facilitate a loading animation
        $('#page-content').addClass('loading');
    });

    router.onSuccess(function (url, dta) {
        // Stop animations in the nav and page-content and scroll to the top of the page in a set amount of time
        setTimeout(function () {
            // Replace old page-content with new page-content
            $('#page-content').html(dta);

            // Add anchor click handlers for internal links in new content loaded into #page-content
            router.jsUrls('#page-content a');

            // Stop page loading
            $('#page-content, #nav').removeClass('loading');

            // Reload any new maths using MathJax
            $('#page-content .math').each(function (math_elem) {
                mj.Hub.Queue(["Typeset", mj.Hub, math_elem[0]]);
            });

            // Add fullscreen functionality to inline-images and figures
            $('article.post p > img').click(function () {
                $(this).get(0).toggleFullScreen();
            });
            $('figure').click(function () {
                $(this).children('img').get(0).toggleFullScreen();
            });

            // Scroll to top of the page
            if ($('body').scrollTop() > $('#nav').offset().top - 15) {
                $('html, body').animate({
                    scrollTop: $('#nav').offset().top - 15
                }, 'fast');
            }
        }, 250);
    });

    router.onError(function (url, status) {
            /* Remove .loading from #page-content and #nav to stop the loading
             * animation. Finally, display an error message in #status.
             */
            $('#page-content, #nav').removeClass('loading');

            // TODO: instead of immediately displaying error, check if the content is stored in local storage
            $('#status').prepend('<p class="message">Error retrieving page ' + url + '</p>');
            $('#status').addClass('error').slideDown();
    });

    site.init();
})(jQuery, MathJax);

// Modified from: https://developer.mozilla.org/en-US/docs/Web/Guide/API/DOM/Using_full_screen_mode
function toggleFullScreen() {
    if (!document.fullscreenElement &&    // alternative standard method
        !document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement ) {  // current working methods
        if (document.documentElement.requestFullscreen) {
            this.requestFullscreen();
        } else if (document.documentElement.msRequestFullscreen) {
            this.msRequestFullscreen();
        } else if (document.documentElement.mozRequestFullScreen) {
            this.mozRequestFullScreen();
        } else if (document.documentElement.webkitRequestFullscreen) {
            this.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
        }
    } else {
        if (document.exitFullscreen) {
            document.exitFullscreen();
        } else if (document.msExitFullscreen) {
            document.msExitFullscreen();
        } else if (document.mozCancelFullScreen) {
            document.mozCancelFullScreen();
        } else if (document.webkitExitFullscreen) {
            document.webkitExitFullscreen();
        }
    }
}

Element.prototype.toggleFullScreen = toggleFullScreen;
