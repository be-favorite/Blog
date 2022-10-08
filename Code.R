# help(package = "distill")

# 포스트 만들기: index.qmd로 수정 필수
distill::create_post(
    "Linux command ", # mandatory
    author = "auto",
    slug = "auto", # generates a website slug (URL)
    date_prefix = TRUE, # adds date for sorting
    draft = TRUE, # 초고 작성시, TRUE
    edit = interactive()
)

# 포스팅 이름 바꾸기
# distill::rename_post_dir("_posts/2022-06-08-2022-4",
#                          slug = "Monthly memory - 202204")