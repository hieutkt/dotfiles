# Import
using Revise
using OhMyREPL; colorscheme!("GruvboxDark")

# Eviroment
push!(LOAD_PATH, homedir()*"/Dropbox/Codes/Julia")

# To make @edit macro works within emacs `julia-repl-mode`
if haskey(ENV, "INSIDE_EMACS")
    ENV["JULIA_EDITOR"] = "emacsclient"
end

# Functions
function Base.show(s::Type)
    col = isconcretetype(s) ? :yellow : :blue
    col = isprimitivetype(s) ? :red : col
    printstyled(string(s); color=col)
    if ismutable(s)
        printstyled(" (mutable)"; color=:cyan)
    end
end

function subtypetree(roottype, prefix = "")
    stype = subtypes(roottype)
    if prefix == ""
        if length(stype) == 0
            show(roottype)
            print(" doesn't have any subtype, and is a subtype of ")
            show(supertype(roottype))
        else
            show(roottype)
        end
    end
    for i in 1:length(stype)
        print("\n")
        printstyled(prefix, i == length(stype) ? "└── " : "├── "; color=:light_black)
        s = stype[i]
        show(s)
        subtypetree(s, "│   " * prefix)
    end
end
