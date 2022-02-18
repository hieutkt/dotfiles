using Revise
using OhMyREPL; colorscheme!("GruvboxDark")

push!(LOAD_PATH, homedir()*"/Dropbox/Codes/Julia")

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
        print(prefix, i == length(stype) ? "└── " : "├── ")
        s = stype[i]
        show(s)
        subtypetree(s, "│   " * prefix)
    end
end
