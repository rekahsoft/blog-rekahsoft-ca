---
title: Site Updates and AppCache depreciation
author: Collin J. Doering
date: Feb 08, 2016
description: Stabilized site and removed javascript requirement
tags: general
---

I am happy to announce that this site is nearly complete. There are a few additional features I
still intend to add, mainly to do with offline viewing and user experience, but all-in-all I am
quiet happy with it. Now I intend to mainly focus on populating this blog with more content and
writing a little more frequently. Also for those of you wondering if pagination is working, it
is, completely! Both on the tag pages and the blog page, however currently I don't have enough
articles to trigger multiple pages (greater then 6 is required), so the pagination
next/prev/first/last page links at the bottom of the page are plain text. I at some point may
address this but it will only really be an issue for another few articles and could be resolved
quiet easily in the case of 6 or less articles.

Recently, I came across an article saying to my dismay that the Application Cache API is being
depreciated in firefox. I say to my dismay because this site uses Application Cache so it can
be viewed offline, which means I'm going to have to refactor my site for Application Caches
replacement. Some would say offline viewing is overkill for a blog but initially I attempted it
as a learning exercise, but am pretty happy with the end result of a pretty smooth offline
viewing experience.

<!--more-->

Needless to say, while setting up Application Cache, it seemed like it wasn't a very flexible
technology. I was able to make it do what I wanted (for the most part) but things like
selectively caching an article wasn't doable with Application Cache. I still intend on offering
this feature, however I was going to implement it via offline storage as it couldn't be done
dynamically on the client side with Application Cache. There is a better alternative though,
ServiceWorkers which replaces Application Cache provides the flexibility to provide just this;
dynamic caching based on whatever conditions the developer sees fit. In a nut shell,
ServiceWorkers act as a proxy running in a Worker (thus in its own thread) between your web
application and the browser.

Anyways, as I read through the documentation for ServiceWorkers I'll be sure to update my blog
to use them in place of Application Cache. I'm looking forward to experimenting with this new
web spec, and thinks it hold great promise for the future.

As a closing note: one thing that worries me about web applications becoming able to deliver
similar experiences to classical native programs is that of freedom and availability of source
code. Of course, this is why the [AGPL](http://www.gnu.org/licenses/agpl-3.0.html) exists, but
none-the-less, I fear the day when the majority of users simply use a web browser to do their
computing. It will leave them locked to a particular service without the ability to even
reverse engineer the application because all communication can take place via an encrypted
connection.
