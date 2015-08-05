---
title: About Comments
author: Collin J. Doering
date: Jul 30, 2015
description: The current state of comments on blog.rekahsoft.ca
tags: general
---

Currently as some may have noticed, the "See Comments" link under each individual blog post
does nothing. This is because I have been struggling to find a good solution to handle
comments. Of course many people choose to use Disqus. Unfortunately due to the proprietary
nature of Disqus, I refuse to use it (See
[this](http://blog.irukado.org/2013/12/disqus-considered-harmful/)). There are some open source
solutions including:

[Isso](http://posativ.org/isso/)
  ~ - Written in Python.
    - Supports modern browsers (IE10+, Firefox, Chrome, ...)

[Juvia](https://github.com/phusion/juvia)
  ~ - Written in Ruby
    - Not maintained

There are quite a few others, though I haven't spent much time investigating them as many of
them are incomplete or unmaintained. See <https://news.ycombinator.com/item?id=6818416> for a
decent discussion of "Disqus Alternatives".

I've tried Isso but because my blog is a Single Page Application (SPA) Isso doesn't function
correctly. Isso expects the current URL to be a direct link to the post html file that is being
commented on, but in the case of my blog it is a virtual url (Eg. this post,
<http://blog.rekahsoft.ca/#/posts/about-comments.html> doesn't work with Isso but
<http://blog.rekahsoft.ca/posts/about-comments.html> would but links directly to the html snippet
file instead of the entire post page.Forgive my vague explanation, its been a few months since
I last played with Isso. Its too bad I couldn't get it to function because from all the open
source options, it seems to be the most mature solution.

Another option that came to mind is to use a sub-reddit for my blog and post new threads for
each blog post as a way for people to submit comments. I don't like this as the content
would then not be hosted on my servers and it would require users to create a account on
Reddit. Not to mention the process wouldn't be simple to automate so I'd have to go create a new
reddit thread in my sub-reddit every time I submitted a new post.

The last option I've considered is just foregoing comments all together. Personally, I rarely
leave comments on blogs and if I'm interested in getting a hold of someone I generally prefer
to use email.

What do you think? Feel free to email me at <collin.doering@rekahsoft.ca> with any comments
and/or suggestions. Anyways, over the next while I hope to find a solution, but for the time
being use email or IRC if you wish to contact me.
