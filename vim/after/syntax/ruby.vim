if !has('conceal')
    finish
endif

syntax match rubyControl "\<or\>"   conceal cchar=∨
syntax match rubyControl "\<and\>"  conceal cchar=∧
syntax match rubyControl "\<not \>" conceal cchar=¬
syntax match rubyControl "not "     conceal cchar=¬
syntax match rubyControl "<="       conceal cchar=≤
syntax match rubyControl ">="       conceal cchar=≥
syntax match rubyControl "=="       conceal cchar=≡
syntax match rubyControl "!="       conceal cchar=≠
syntax match rubyControl "=\~"      conceal cchar=≅
syntax match rubyControl "!\~"      conceal cchar=≆
syntax match rubyControl "\->"      conceal cchar=↦
syntax match rubyControl "=>"       conceal cchar=⇒

hi! link Conceal           Operator

set conceallevel=2
