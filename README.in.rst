|build-status| |melpa-dev|

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://github.com/dickmao/nndiscourse/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/nndiscourse/actions
   :alt: Build Status
.. |melpa-dev|
   image:: https://melpa.org/packages/nndiscourse-badge.svg
   :target: http://melpa.org/#/nndiscourse
   :alt: MELPA current version

.. image:: https://github.com/dickmao/gnus-imap-walkthrough/blob/master/thumbnail.png
   :target: https://youtu.be/DMpZtC98F_M
   :alt: Replacing Thunderbird With Gnus

.. image:: screenshot.png
.. |--| unicode:: U+2013   .. en dash
.. |---| unicode:: U+2014  .. em dash, trimming surrounding whitespace
   :trim:

Does not work for sites requiring login
=======================================
Some discourse instances allow unfettered public viewing, e.g.,
``emacs-china.org``, ``devforum.roblox.com``.  Others require login, e.g.,
``discourse.doomemacs.org``.  At the time I wrote nndiscourse, it was
impossible to get login going, and while `it does seem possible now
<https://meta.discourse.org/t/user-api-keys-specification/48536/33?u=dickmao>`_,
it still looks really hard and undocumented.

Install
=======
Alas, you'll need Cask_. Then,

::

   rbenv install 2.6.2
   git clone https://github.com/dickmao/nndiscourse.git
   make -C nndiscourse install

Usage
=====
Suppose you want to follow https://emacs-china.org.  In your ``.emacs`` or ``init.el``, use ONE of the following:

::

   ;; Applies to first-time Gnus users
   (custom-set-variables '(gnus-select-method
       (quote (nndiscourse "emacs-china.org" (nndiscourse-scheme "https")))))

or, if you're an existing Gnus user,

::

   ;; Applies to existing Gnus users
   (add-to-list 'gnus-secondary-select-methods
       (quote (nndiscourse "emacs-china.org" (nndiscourse-scheme "https"))))

Then ``M-x gnus``.

Select a topic category via ``RET``.  Rapidly catch yourself up via ``N`` and ``P``.  Instantly catch-up with ``c``.

From the ``*Group*`` buffer, press ``g`` to refresh all categories.  ``M-g`` on a particular category to refresh individually.

From the summary buffer, ``/o`` redisplays posts already read.  ``x`` undisplays them.

Gnus beginners may find the interface bewildering.  In particular, categories with no unread posts do not display.  Use ``L`` to bring them out of hiding.

.. _Cask: https://github.com/cask/cask
.. _Getting started: http://melpa.org/#/getting-started
