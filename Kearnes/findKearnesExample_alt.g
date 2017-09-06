# FILE: findKearnesExample.g
# AUTHOR: William DeMeo
# DATE: 2017.09.05
# UPDATED: 2017.09.05
#
# DESCRIPTION: GAP routines for creating an example of interest to Keith Kearnes
# Specifically, Keith wants a group G satisfying the following, for some 
# subgroup Q < G:
#                     (1)  [G,G]=G
#                     (2)  [Q,Q] \neq 1.
#                     (3)  [[Q,G],G] = 1.
#
# ISSUES: send questions, comments, suggestions to 
#         [William DeMeo](mailto:williamdemeo@gmail.com)
#

findExampleInG_alt:=function(G, i, j)
    # Check whether the given group has a subgroup Q satisfying the 3 conditions above.
    local count_2, D, e, ccsg, numccs, foundOne, m, Q, cqq, cqg, ccqgg, flag;
    
    flag:=false;  # (merely to avoid redundant printing)
    
    count_2:=0;
    e := Group([Identity(G)]);
    
    ccsg := ConjugacyClassesSubgroups(G);
    numccs := Size(ccsg);
    foundOne := false;
            
    for m in [2..(numccs-1)] do
        
        if not foundOne then     # (find at most one example)
                    
            Q := Representative(ccsg[m]);
                
            #if IsAbelian(Q) then
                # do nothing
            #else  
                # Condition (2) is satisfied!!
                
            cqg := CommutatorSubgroup(Q,G);
            
            if Size(cqg) = 1 then
                # do nothing
            else
                count_2:= count_2+1;
                
                if not flag then  # only print index of G if we haven't done so already 
                    Print("\n------------------------------------------------------------");
                    Print("\n G := SmallGroup(", i, ", ", j, ") = ", StructureDescription(G), "\n");
                    flag := true;
                fi;
                
                Print("\n         Q := Rep(ccsg[", m, "]) = ", StructureDescription(Q));
             
                cqq := DerivedSubgroup(Q);
                                       
                Print("               [Q, Q] = ", StructureDescription(cqq), "             [Q, G] = ", StructureDescription(cqg), "    \n");
                
                ccqgg := CommutatorSubgroup(cqg,G);
                    
                if Size(ccqgg)=1 then
                        
                    # Condition (3) is satisfied!!
                                    
                    foundOne := true;
                    Print("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                    Print("\n >>>> WINNER! >>>>  Q = Representative(ccsg[", m, "])  = ", StructureDescription(Q), "\n");
                    Print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n");
                    return [Q, count_2];
                            
                fi;
                    
            fi;
                    
        fi;
                
    od;
    
        
    # if foundOne then
    #     return [G, Q];  # [G, Q] is an example
    # else
    #     return [G, e];  # signifies that G is not an example.
    # fi;
    
    return [e, count_2];  # signifies that G is not an example.
end;


findKearnesExample:=function(N1, N2)
    # GAP function for finding a group G with a subgroup Q satisfying
    # 
    #                     (1)  [G,G]=G
    #                     (2)  [Q,Q] \neq 1.
    #                     (3)  [[Q,G],G] = 1.
    #
    # INPUT  
    #        (N1, N2) search among all groups G of order |G| between N1 and N2.
    #
    local i, j, qc2, Q, count_i, count_tot, numsg, G, D, count_1, count_2, VERBOSE;
    
    VERBOSE:=false;  ## set this to true for verbose output
    
    count_1 := 0;
    count_2 := 0;
    count_tot := 0;
                   
    for i in [N1..N2] do
        
        count_i:=0;
        
        numsg := NumberSmallGroups(i);
        
        if VERBOSE then
            Print("Checking the ", numsg, " group(s) of order |G| = ", i, "...\n");
        fi;
        
                   
        for j in [1..numsg] do
            
            G := SmallGroup(i,j);
            # Print("(", i, ", ", j, ")   ");
            
            # First check whether G satisfies condition (1) [G, G] = G.
            
            # Checking Abelian is probably much faster than computing the derived subgroup [G, G], so first...
            if IsAbelian(G) then
                # do nothing  
            else
        
                D:=DerivedSubgroup(G);
   
                if (Size(D) < Size(G)) then
                    # do nothing
                else  
                    # Condition (1) is satisfied!
                    count_1 := count_1 + 1;
                    
                    # qc2 := findExampleInG(G, i, j);
                    qc2 := findExampleInG_alt(G, i, j);
                    
                    Q := qc2[1];  count_2:= count_2 + qc2[2];
                    
            
                    if (Size(Q) > 1) then
                        count_i:= count_i+1;
                        count_tot:= count_tot+1;
                        ## found an example!  write it to output file here
                    fi;
                    
                fi;
            fi;
            
                
        od;
        
        if VERBOSE then
            Print(" ...done!  (found = ", count_i, " examples with |G| = ", i, ";   TOTAL SO FAR: ", count_tot, ", (count_1, count_2) = (", count_1, ", ", count_2, ")\n" );
        fi;
        
        
    od;
    
end;

