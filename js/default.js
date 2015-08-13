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
_paq.push(["setDoNotTrack", true]);
_paq.push(['enableLinkTracking']);

// Asynchronously load piwik.js
(function() {
  var u="//analytics.rekahsoft.ca/";
  _paq.push(['setTrackerUrl', u+'piwik.php']);
  _paq.push(['setSiteId', 1]);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
})();

(function ($, mj) {
    "use strict";

    // The identity function
    function idFun(x) {
        return x;
    }

    var router = (function () {
        var routes = [
            { // Post pages handler
                acceptUrls: /posts\/.*\.html/,
                rewriteGetUrl: idFun,
                rewriteVirtualUrl: idFun,
                ajaxCallbacks: {
                    beforeSend: function () {
                        $('#nav-menu li.active').removeClass('active');
                        $('#nav-menu li a[href="./blog.html"]').parent('li').addClass('active');
                    }
                }
            },
            { // Tag pages handler
                acceptUrls: /tags\/.*(\d*)\.html/,
                rewriteGetUrl: function (url) {
                    var tag_not_regexp = /(tags\/.*[^\d]+)(\.html)/;
                    if (tag_not_regexp.test(url)) {
                        return url.replace(tag_not_regexp, "$11$2");
                    }
                    return url;
                },
                rewriteVirtualUrl: function (url) {
                    var tag_one_regexp = /(tags\/.*)1(\.html)/;
                    if (tag_one_regexp.test(url)) {
                        return url.replace(tag_one_regexp, "$1$2");
                    }
                    return url;
                },
                ajaxCallbacks: {
                    beforeSend: function () {
                        $('#nav-menu li.active').removeClass('active');
                        $('#nav-menu li a[href="./blog.html"]').parent('li').addClass('active');
                    }
                }
            },
            { // Blog pages handler
                acceptUrls: /blog\d*\.html/,
                rewriteGetUrl: function (url) {
                    if (url === "/blog.html") {
                        url = "/blog1.html";
                    }
                    return url;
                },
                rewriteVirtualUrl: function (url) {
                    if (url === "/blog1.html") {
                        url = "/blog.html";
                    }
                    return url;
                },
                ajaxCallbacks: {
                    beforeSend: function () {
                        // Set the blog menuitem as active
                        $('a.menuitem[href="./blog.html"]').closest('ul').find('li.active').removeClass('active');
                        $('a.menuitem[href="./blog.html"]').closest('li').addClass('active');
                    }
                }
            },
            { // Default page handler
                acceptUrls: /.*/,
                rewriteGetUrl: function (url) {
                    if (url === "/") {
                        url = "/index.html";
                    }
                    return url;
                },
                rewriteVirtualUrl: function (url) {
                    if (url === "/index.html") {
                        url = "/";
                    }
                    return url;
                },
                ajaxCallbacks: {
                    beforeSend: function (url, virt_url) {
                        if (virt_url === "/") {
                            virt_url = "/index.html";
                        }

                        // Initially set the active menuitem in the nav
                        $('a.menuitem[href="' + virt_url + '"]').closest('ul').find('li.active').removeClass('active');
                        $('a.menuitem[href="' + virt_url + '"]').closest('li').addClass('active');
                    }
                }
            }],
            callback = idFun,

            spec = {
                runRouter: function runRouter(url) {
                    function runRouter_help(spec) {
                        var i, new_virt_url;
                        for (i = 0; i < routes.length; i += 1) {
                            if (routes[i].acceptUrls.test(spec.url)) {
                                new_virt_url = routes[i].rewriteVirtualUrl(spec.url);
                                if (new_virt_url === spec.url) {
                                    if (spec.hasRedirect) {
                                        // TODO: use history API in place of $.address (from jquery-address)
                                        history.pushState(null, "Title", new_virt_url);
                                    } else {
                                        callback(routes[i].rewriteGetUrl(spec.url), spec.url, routes[i].ajaxCallbacks);
                                    }
                                } else if (spec.numRecur <= spec.recurDepth) {
                                    runRouter_help({ url: new_virt_url,
                                                     hasRedirect: true,
                                                     numRecur: spec.numRecur + 1,
                                                     recurDepth: spec.recurDepth });
                                } else {
                                    console.log("Exceeded recursion depth for router");
                                }
                                break;
                            }
                        }
                    }

                    runRouter_help({ url: url,
                                     hasRedirect: false,
                                     numRecur: 1,
                                     recurDepth: 5 });
                },
                setCallback: function setCallback(cb) {
                    if (typeof cb === 'function') {
                        callback = cb;
                    }
                }
            };

        return spec;
    }()),

        page = (function () {
            // var pageId = '#page-content', navId = '#nav';

            function loadPageContent(page_href, virt_href, handlerCallback) {
                // Track page view with piwik
                _paq.push(['setDocumentTitle', document.domain + '/' + virt_href]);
                _paq.push(['trackPageView']);

                $.ajax({
                    url: page_href,
                    type: 'GET',
                    dataType: 'html',
                    beforeSend: function (xhr, settings) {
                        // Add .loading to #page-content and #nav to facilitate a loading animation
                        $('#page-content, #nav').removeClass('loading-done').addClass('loading');

                        // Run current handlers onSuccess callback (if it exists)
                        if (handlerCallback.hasOwnProperty('beforeSend') && typeof handlerCallback.beforeSend === 'function') {
                            handlerCallback.beforeSend(page_href, virt_href);
                        }

                        console.log('beforeSend a.menuitem');
                    },
                    success: function (dta) {
                        // Remove any status message errors or successes
                        $('#status').slideUp('normal', function () {
                            $('#status').removeClass('error').removeClass('success').children('p.message').remove();
                        });

                        // Stop animations in the nav and page-content and scroll to the top of the page in a set amount of time
                        setTimeout(function () {
                            // Replace old page-content with new page-content
                            dta = $($.parseHTML(dta)).filter('#page-content').html();
                            $('#page-content').html(dta);

                            // Stop page loading
                            $('#page-content, #nav').removeClass('loading');

                            // Reload any new maths using MathJax
                            $('#page-content .math').each(function (math_elem) {
                                mj.Hub.Queue(["Typeset", mj.Hub, math_elem[0]]);
                            });

                            // Rewrite new URLs within new content inserted into #page-content
                            $('#page-content a').each(function (i) {
                                var href = $(this).attr('href'),
                                    external_url_regexp = /https?:\/\/.*/,
                                    mailto_regexp = /mailto:.*/,
                                    files_regexp = /files\/.*/,
                                    images_regexp = /images\/.*/;

                                if (!(external_url_regexp.test(href) || mailto_regexp.test(href) || files_regexp.test(href) || images_regexp.test(href))) {
                                    $(this).attr('href', "/#" + href);
                                }
                            });

                            // Add fullscreen functionality to inline-images and figures
                            $('article.post p > img').click(function () {
                                $(this).get(0).toggleFullScreen();
                            });
                            $('figure').click(function () {
                                $(this).children('img').get(0).toggleFullScreen();
                            });

                            // Run current handles onSuccess callback (if it exists)
                            if (handlerCallback.hasOwnProperty('onSuccess') && typeof handlerCallback.onSuccess === 'function') {
                                handlerCallback.onSuccess();
                            }

                            // Scroll to top of the page
                            if ($('body').scrollTop() > $('#nav').offset().top - 15) {
                                $('html, body').animate({
                                    scrollTop: $('#nav').offset().top - 15
                                }, 'fast');
                            }
                        }, 250);
                    },
                    error: function (xhr, status) {
                        /* Remove .loading from #page-content and #nav to stop the loading
                         * animation. Finally, display an error message in #status.
                         */
                        $('#page-content, #nav').removeClass('loading');

                        // TODO: instead of immediately displaying error, check if the content is stored in local storage
                        $('#status').prepend('<p class="message">Error retrieving page ' + page_href + '</p>');
                        $('#status').addClass('error').slideDown();

                        // Run current handles onError callback (if it exists)
                        if (handlerCallback.hasOwnProperty('onError') && typeof handlerCallback.onError === 'function') {
                            handlerCallback.onError();
                        }
                    }
                });
            }

            function init(router) {
                router.setCallback(loadPageContent);

                $(document).ready(function () {
                    $('#nav-menu a.menuitem').click(function () {
                        $(this).closest('ul').find('li.active').removeClass('active');
                        $(this).closest('li').addClass('active');
                        //$('.navbar-collapse').collapse('hide');
                    });

                    $('#status a.close-button').click(function () {
                        $(this).parent().slideUp(function () {
                            $(this).removeClass('error').removeClass('success');
                            $(this).children('p.message').remove();
                        });
                    });

                    // TODO: use history API in place of $.address.change (from jquery-address)
                    // Callback for when the inital page has completely loaded (including images, etc..)
                    //$.address.change(function (event) {
                    //    console.log("Change " + event.value);
                    //    router.runRouter(event.value);
                    //}); // TODO
                });
            }

            var spec = {
                init: init
            };
            return spec;
        }());

    page.init(router);
}(jQuery, MathJax));

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
