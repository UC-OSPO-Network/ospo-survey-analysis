# Lessons learned

For anyone who is looking to replicate our survey, or parts of it, please
consider improving on our survey instrument with these “lessons learned”.
Hindsight is 20/20…

## General thoughts

- Making nearly all questions mandatory was a good choice–it makes the
  statistics much easier.
- Make sure that the user experience is good for non-contributors, aspiring
  contributors, and non-UC. We got more of these folks than I expected, so I
  wasn’t really paying close attention to the UX for those groups.
- Do we want to ask questions about policy and culture?
- We never asked an obvious question: how often do you contribute to open
  source? If we asked that question, maybe we could then ask Q5 about project
  sizes in a simpler way, e.g. ask for absolute rather than relative
  frequencies.
- This is a really small thing, but be consistent about capitalization and
  plurals in survey choices, and check spelling. For example, I wish we had said
  “Graduate Student” and “Undergraduate Student” instead of “Grad Student” and
  “Undergraduate”, and “Postdoc” instead of “Post-Doc”, and that we had used
  consistent capitalization for “Non-research Staff” and “Other research staff”.
  It just makes the analysis easier.
- There is a demographic of people who used to do OS work in industry. Consider
  rereading the survey with these folks in mind, and make sure they’re able to
  answer all Qs.
- In the future, it would be cool to have a list of projects universities
  commonly use/contribute to, and ask people if they’ve ever used and/or
  contributed to that tool. (jQuery, Drupal, etc.) Ask Patrick Masson of Apereo
  about this—he has some preliminary data.

## Specific Qs

- Maybe Q2 should have had an “I don’t know” option
- For Q2, perhaps we should have directly asked researchers whether they feel
  contributing to open source is part of their job. As it is, just because
  people didn’t check that box doesn’t mean they don’t feel it’s true. It just
  means it’s not a motivation for contributing to open source.
- For Q2, there is ambiguity between “learning” and “professional development”,
  especially for non-students. Dozens of people put “Non-applicable” for
  “Learning”. We should clarify the difference between “Learning” and
  “Professional development”.
- On Q4 about roles, other things people wrote in were Conference Organizer,
  Fundraiser, and Ambassador/Outreach/Advocate
- For Q6 about motivations, we got the following additional themes from the
  comments: “to take power back from profit-driven corporations”, “it’s aligned
  with academic values”, it’s good for my career”, “it saves money”, and “to
  contribute to research”. We might also re-word the “give back” option so that
  it’s clear whether they are giving back to OS users/devs or to society at
  large?
- For Q6, “To improve tools in my field” and “To customize existing tools for my
  specific needs” are so similar that I feel like it almost violates my
  statistical model's assumption that all outcomes are independent.
- For Q7 about project types, some people wrote in documentation, educational
  resources, datasets, operating systems, or “research code”. I think more
  clarification is needed for what is meant by “frameworks”. Consider adding
  examples for each of these. Also consider splitting out front-end web code
  from backend stuff: databases and/or database management systems.
- On Q8 about hosting platforms, we should have said where do you “publicly”
  share your code, since some people mentioned things like email and google
  drive. Here are some other platforms we might consider, based on things people
  wrote or things that I just thought of later:
  - Kaggle
  - Huggingface
  - CodeOcean
  - Wolfram Notebook Archive
  - A linux package manager such as apt, yum, or aur
  - PyPi
  - CRAN
  - A 3D printing design sharing platform such as Printables or Thingiverse
  - A private or institutional git server (Not sure if they were thinking about
    open, public code, but 4 people wrote this in)
  - Also no one chose Vivli, maybe remove that one
  - Remove the “multiple answers should be comma separated”?
- Maybe separate Q8 into separate Qs for software and hardware. If someone has
  contributed to hardware projects and they select “zenodo”, we don’t know if
  they’re sharing code or design files.
- For Q9 about challenges, “Finding time to educate myself” ought to be “Limited
  time to educate myself”, for consistency with earlier options.
- For the solutions, we should have clarified whether “free, feature-rich
  computing environments” includes storage. In fact, we should have had an
  additional question about computing needs, as this was a super popular option.
- I think it would have been good to get qualitative feedback from Q11,
  something like, “please explain why you chose the solution you chose”.
- For Q13, a lot of people volunteered their repos. In the future, we should
  give them space to enter a specific repo, if they want to. We should also make
  Qualtrics enforce the “string:string, string:string” format.
- Instead of asking aspiring contributors, “what would make you more likely to
  participate” as a select all that apply question, we should make it a matrix
  with not useful/useful/very useful, like Q10, and make the options as similar
  as possible to the corresponding Q (Q10) for experienced contributors. We
  could even just show both groups the same question, since there is a N/A
  option.
- We probably should have asked aspiring contributors for their free-response
  comments on challenges and solutions.
- Q15, aspiring contributors: remove the “Multiple answers should be
  comma-separated” part.
- Consider something like “not knowing where to contribute” as a barrier for
  aspiring contributors.
- Instead of asking participants to write in their academic discipline, ask them
  to choose their tier three discipline from the three tiered digital commons
  taxonomy: https://digitalcommons.elsevier.com/en_US/dc-disciplines-taxonomy. I
  would recommend, however, that we make the following substitution: 1017,
  “Artificial Intelligence and Robotics”, should be replaced with “Artificial
  Intelligence and Machine Learning”. Robotics is represented on its own
  elsewhere. AI is not.
- To Q19, staff work areas, add “Business Systems Analyst”. 2 people wrote this
  in.
