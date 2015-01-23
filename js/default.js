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

(function ($, mj) {
    "use strict";

    var page = (function () {
        // var pageId = '#page-content', navId = '#nav';

        function loadPageContent(page_href, virt_href, handlerCallback) {
            $.ajax({
                url: page_href,
                type: 'GET',
                dataType: 'html',
                beforeSend: function (xhr, settings) {
                    // Remove loading error from page-content and any status message errors
                    $('#page-content').removeClass('loading-error');
                    $('#status').slideUp('normal', function () {
                        $('#status').removeClass('error').removeClass('success');
                    });

                    // Add .loading to #page-content and #nav to facilitate a loading animation
                    $('#page-content, #nav').removeClass('loading-done').addClass('loading');

                    // Run current handlers onSuccess callback (if it exists)
                    if (handlerCallback.hasOwnProperty('beforeSend') && typeof handlerCallback.beforeSend === 'function') {
                        handlerCallback.beforeSend(page_href, virt_href);
                    }

                    console.log('beforeSend a.menuitem');
                },
                success: function (dta) {
                    // Remove the initial loading gif (if its there)
                    $('#page-content').removeClass('init');

                    // Stop animations in the nav and page-content and scroll to the top of the page in a set amount of time
                    setTimeout(function () {
                        // Replace old page-content with new page-content
                        $('#page-content').html(dta);

                        // Stop page loading
                        $('#page-content, #nav').removeClass('loading');

                        // Reload any new maths using MathJax
                        $('#page-content .math').each(function (math_elem) {
                            mj.Hub.Queue(["Typeset", mj.Hub, math_elem[0]]);
                        });

                        $('#page-content a').each(function (i) {
                            var page_href = $(this).attr('href'),
                                external_url_regexp = /https?:\/\/.*/,
                                mailto_regexp = /mailto:.*/,
                                files_regexp = /files\/.*/,
                                images_regexp = /images\/.*/;

                            if (!(external_url_regexp.test(page_href) || mailto_regexp.test(page_href) || files_regexp.test(page_href) || images_regexp.test(page_href))) {
                                $(this).attr('href', "/#" + page_href);
                            }
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
                     * animation. Then add .loading-error to #page-content if its the sites
                     * first load (#page-content has class .init). Finally, display an error
                     * message in #status.
                     */
                    $('#page-content, #nav').removeClass('loading');
                    if ($('#page-content.init')[0]) {
                        $('#page-content').addClass('loading-error').html('<p class="container border-box">Error initially loading blog.rekahsoft.ca. Check the url! Given "' + page_href + '"</p>');
                    } else {
                        $('#status > p.message').text('Error retrieving page "' + page_href + '": ' + status);
                        $('#status').addClass('error').slideDown();
                    }

                    // Run current handles onError callback (if it exists)
                    if (handlerCallback.hasOwnProperty('onError') && typeof handlerCallback.onError === 'function') {
                        handlerCallback.onError();
                    }
                }
            });
        }

        function runHandlers (url, handlers) {
            for (var i = 0; i < handlers.length; i++) {
                if (handlers[i].acceptUrls.test(url)) {
                    var new_virt_url = handlers[i].rewriteVirtualUrl(url);
                    if (new_virt_url === url) {
                        loadPageContent(handlers[i].rewriteGetUrl(url), url, handlers[i].ajaxCallbacks);
                    } else {
                        $.address.value(new_virt_url);
                    }
                    break;
                }
            }
        }
        
        function init (handlers) {
            $(document).ready(function () {
                $('#nav-menu a.menuitem').click(function () {
                    $(this).closest('ul').find('li.active').removeClass('active');
                    $(this).closest('li').addClass('active');
                    //$('.navbar-collapse').collapse('hide');
                });

                $('#status a.close-button').click(function () {
                    $(this).parent().slideUp();
                });

                // Callback for when the inital page has completely loaded (including images, etc..)
                $.address.change(function (event) {
                    console.log("Change " + event.value);
                    runHandlers(event.value, handlers);
                });
            });
        }

        return {
            init: init
        };
    }());

    function idRewrite (url) {
        return url;
    }

    var handlers = [
        { // Post pages handler
            acceptUrls: /posts\/.*\.html/,
            rewriteGetUrl: idRewrite,
            rewriteVirtualUrl: idRewrite,
            ajaxCallbacks: {
                beforeSend: function () {
                    $('#nav-menu li.active').removeClass('active');
                    $('#nav-menu li a[rel="address:/blog.html"]').parent('li').addClass('active');
                }
            }
        },
        { // Tag pages handler
            acceptUrls: /tags\/.*(\d*)\.html/,
            rewriteGetUrl: function (url) {
                var tag_not_regexp = /(tags\/.*[^\d]+)(\.html)/;
                if (tag_not_regexp.test(url)) {
                    return url.replace(tag_not_regexp, "$11$2");
                } else {
                    return url;
                }
            },
            rewriteVirtualUrl: function (url) {
                var tag_one_regexp = /(tags\/.*)1(\.html)/;
                if (tag_one_regexp.test(url)) {
                    return url.replace(tag_one_regexp, "$1$2");
                } else {
                    return url;
                }
            },
            ajaxCallbacks: {
                beforeSend: function () {
                    $('#nav-menu li.active').removeClass('active');
                    $('#nav-menu li a[rel="address:/blog.html"]').parent('li').addClass('active');
                }
            }
        },
        { // Blog pages handler
            acceptUrls: /blog\d*\.html/,
            rewriteGetUrl: function (url) {
                if (url === "/blog.html") {
                    url = "/blog1.html"
                }
                return "pages" + url;
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
                    $('a.menuitem[rel="address:/blog.html"]').closest('ul').find('li.active').removeClass('active');
                    $('a.menuitem[rel="address:/blog.html"]').closest('li').addClass('active');
                }
            }
        },
        { // Default page handler
            acceptUrls: /.*/,
            rewriteGetUrl: function (url) {
                if (url === "/") {
                    url = "/home.html";
                }
                return "pages" + url;
            },
            rewriteVirtualUrl: idRewrite,
            ajaxCallbacks: {
                beforeSend: function (url, virt_url) {
                    console.log('setting active menu item to ' + url);
                    // Initially set the active menuitem in the nav
                    $('a.menuitem[rel="address:' + virt_url + '"]').closest('ul').find('li.active').removeClass('active');
                    $('a.menuitem[rel="address:' + virt_url + '"]').closest('li').addClass('active');
                }
            }
        }
    ];

    // Initialize page with handlers
    page.init(handlers);
}(jQuery, MathJax));
