subroutine xprfastcalcul(jvtemp, nbnoma, jcalculs, jnodto, nbno, jcnsls, &
                         cnxinv, jconx1, jconx2, ndim, jcopiels, noma)

   implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnscno.h"
#include "asterfort/getvr8.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xdecoupe.h"
#include "asterfort/xsolveurtria.h"
#include "asterfort/xvaleurmin.h"

    integer           :: jvtemp, nbnoma, jcalculs, jnodto, nbno
    integer           :: jcnsls, jconx1, jconx2
    integer           :: ndim, jcopiels  
    character(len=19) :: cnxinv
    character(len=8)  :: noma

    
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: patrick.massin at edf.fr

!   nombre max de ss_element que l'on peut obtenir(hexa)
    integer, parameter                :: ss_elem_max = 144
    character(len=8)                  :: elp   
                
    integer                           :: ifm, niv
    integer                           :: cnset(ss_elem_max*4), nse, nnose    
    integer                           :: i, j, k, ise, nbr, minlo, node, node_sselm        
    integer                           :: nbelno, jelno, numelm
    integer                           :: itypma, eldim, jaux, upd_node, indmax    
    real(kind=8)                      :: delta, p, detT, solution, term(3)
    real(kind=8),dimension(ndim)      :: D, Id, dist, n
    real(kind=8),dimension(ndim+1)    :: phi, x, y, z
    real(kind=8),dimension(ndim,ndim) :: V, T ,coor_nod, invT
    integer, pointer                  :: mai(:) => null()
    integer, pointer                  :: tmdim(:) => null()
    real(kind=8), pointer             :: vale(:) => null()
    aster_logical                     :: pres

!-------------------------------------------------------------------------------------------!  
!                             RECUPERATION DES OBJETS JEVEU                                 ! 
!-------------------------------------------------------------------------------------------!    
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)

!   RETRIEVE THE TYPE OF EACH ELEMENT IN THE MESH
    call jeveuo(noma//'.TYPMAIL', 'L', vi=mai)    

!   RETRIEVE THE DEFINITION OF THE ELEMENTS IN TERMS OF NODES
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    
!   RETRIEVE THE DIMENSIONS OF THE EXISTING ELEMENTS
    call jeveuo('&CATA.TM.TMDIM', 'L', vi=tmdim)
    
!   RETRIEVE THE COORDINATES OF THE NODES
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)   
    
!------------------------------------------------------------------------------------------!    
!                                DEBUT                                                     !    
!------------------------------------------------------------------------------------------!

    do while ( any(zl(jvtemp:jvtemp+nbnoma-1)) .eqv. .true. )

!       Recherche la valeur minimal dans le vecteur calculs
        call xvaleurmin(jcalculs,jvtemp,jnodto,nbno,minlo)

!       Ajoute la valeur du noeud min dans le champ LS
        zr(jcnsls-1+zi(jnodto-1+minlo)) = zr(jcalculs-1+zi(jnodto-1+minlo))   

!       Nombre de maille rattaché au noeud       
        call jelira(jexnum(cnxinv, zi(jnodto-1+minlo)), 'LONMAX', nbelno)
        call jeveuo(jexnum(cnxinv, zi(jnodto-1+minlo)), 'L', jelno)

        do j = 1, nbelno

!           Numero de l'element     
            numelm=zi(jelno-1+j) 

!           WORK ONLY WITH THE ELEMENTS OF THE SAME DIMENSION OF THE MESH
            itypma=mai(numelm)              
            eldim=tmdim(itypma)

!           Test de dimension
            if (eldim .eq. ndim) then  
 
!               RETRIEVE THE NUMBER OF NODES FORMING THE ELEMENT
                call jeveuo(jexnum('&CATA.TM.NBNO' , itypma), 'L', jaux)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DECOUPAGE DES ELEMENTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!               Decoupe des éléments en tétraèdre
                if ( ndim .eq. 3) then
                    if ( zi(jaux) .eq. 8 ) then
                        elp = 'HE8'
                        call xdecoupe(elp, cnset, nse, nnose)
                    elseif ( zi(jaux) .eq. 6 ) then   
                        elp = 'PE6'
                        call xdecoupe(elp, cnset, nse, nnose)
                    elseif ( zi(jaux) .eq. 5 ) then   
                        elp = 'PY5'                    
                        call xdecoupe(elp, cnset, nse, nnose)                        
                    elseif ( zi(jaux) .eq. 4) then
                        elp ='TE4'
                        call xdecoupe(elp, cnset, nse, nnose)   
                    else
!                       TYPE D'ELEMENT FINI PAS TRAITE
                        ASSERT(.false.)
                    endif    
                else    
                    if ( zi(jaux) .eq. 4 ) then
                        elp ='QU4'
                        call xdecoupe(elp, cnset, nse, nnose)
                    elseif ( zi(jaux) .eq. 3) then
                        elp ='TR3'
                        call xdecoupe(elp, cnset, nse, nnose)   
                    else
!                       TYPE D'ELEMENT FINI PAS TRAITE
                        ASSERT(.false.)
                    endif    
                endif
                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!BOUCLE PRINCIPALE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!               Boucle sur les sous-elements
                do ise = 1 , nse
                
!                       Variables de test               
                        nbr  = 0
                        pres = .true. 
                        
!                   Boucle sur les noeuds du sous-element                                 
                    do k = 1, nnose
                                        
!                       Numero noeud locale                   
                        node_sselm=cnset(nnose*(ise-1)+k)

!                       Numero noeud globale
                        node = zi(jconx1-1+zi(jconx2-1+numelm)+node_sselm-1)
                        
!                       Verifie que l'on est dans la zone de mise à jour                   
                        if (zr(jcopiels-1+node) .lt. 0) then
                            goto 1000
                        endif

!                       Coordonnée du noeud globale
                        x(k)=vale(3*(node-1)+1)
                        y(k)=vale(3*(node-1)+2)
                        if (ndim .eq. 3 ) then
                            z(k)=vale(3*(node-1)+3)
                        endif
                        
!                       Valeur de la level set au noeud
                        phi(k)=  zr(jcalculs-1+node)

!                       Compte le nombre de noeud à l'infini dans l'élement               
                        if (phi(k) .eq. r8gaem() ) then
                            nbr= nbr + 1
                        endif   
                        
!                       Regarde si le noeud minimum est présent dans le sous-element
                        if (node .eq. zi(jnodto-1+minlo) ) then
                            pres = .false.    
                        endif  
                                                                
                    end do
                                                                              
!                   Pas de mise à jour                
                    if (nbr .ge. 2 .or. pres) cycle

!                   Assemblage de la matrice, du vecteur de distance et du vecteur unite
                    indmax = maxloc(phi,1)
                    k = 1              
                    do i = 1 ,nnose
                        if ( i .ne. indmax) then
                            V(1,k)  = x(i)-x(indmax)
                            V(2,k)  = y(i)-y(indmax)
                            D(k)    = phi(i)
                            Id(k)   = 1
                            if( ndim .eq. 3) then
                                V(3,k)  = z(i)-z(indmax)                                
                                coor_nod(1,k)= x(i)
                                coor_nod(2,k)= y(i)
                                coor_nod(3,k)= z(i)
                                dist(k) = phi(i) + sqrt((x(i)-x(indmax))**2.d0 + &
                                          (y(i)-y(indmax))**2.d0+(z(i)-z(indmax))**2.d0)     
                            else
                                dist(k) = phi(i) + sqrt((x(i)-x(indmax))**2.d0 + &
                                          (y(i)-y(indmax))**2.d0)  
                            endif                          
                            k = k+1
                        endif                   
                    enddo
                    
!                   Noeud a mettre à jour                
                    upd_node = zi(jconx1-1+zi(jconx2-1+numelm)+cnset(nnose*(ise-1)+indmax)-1)       
                                 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CALCUL DES TERMES DE LEQUATION QUADRATIQUE!!!!!!!!!!!!!!!!!!!!!!!!

                    T= matmul(transpose(V),V)
                    
                    if (ndim .eq. 3) then
                       detT = T(1,1)*T(2,2)*T(3,3)+T(2,1)*T(3,2)*T(1,3)+T(3,1)* &
                              T(1,2)*T(2,3)-T(1,1)*T(3,2)*T(2,3)-T(3,1)*T(2,2)* &
                              T(1,3)-T(2,1)*T(1,2)*T(3,3)
                              
                       ASSERT(detT .ne. 0)
                       
!                      Inversion de la matrice T                       
                       invT(1,1) = (1/detT)*(T(2,2)*T(3,3)-T(2,3)*T(3,2))
                       invT(1,2) = (1/detT)*(T(1,3)*T(3,2)-T(1,2)*T(3,3)) 
                       invT(1,3) = (1/detT)*(T(1,2)*T(2,3)-T(1,3)*T(2,2))

                       invT(2,1) = (1/detT)*(T(2,3)*T(3,1)-T(2,1)*T(3,3))
                       invT(2,2) = (1/detT)*(T(1,1)*T(3,3)-T(1,3)*T(3,1)) 
                       invT(2,3) = (1/detT)*(T(1,3)*T(2,1)-T(1,1)*T(2,3))

                       invT(3,1) = (1/detT)*(T(2,1)*T(3,2)-T(2,2)*T(3,1))
                       invT(3,2) = (1/detT)*(T(1,2)*T(3,1)-T(1,1)*T(3,2)) 
                       invT(3,3) = (1/detT)*(T(1,1)*T(2,2)-T(1,2)*T(2,1))    
                       
                    else 
                       detT = T(1,1)*T(2,2)-T(1,2)*T(2,1)
                       
                       ASSERT(detT .ne. 0)      
                       
!                      Inversion de la matrice T                                           
                       invT(1,1) = (1/detT)*T(2,2)
                       invT(1,2) = (-1/detT)*T(1,2)

                       invT(2,1) = (-1/detT)*T(2,1)
                       invT(2,2) = (1/detT)*T(1,1)

                    endif        
                                                                    
                    term(1)=dot_product(Id,matmul(invT,Id))
                    term(2)=dot_product(Id,matmul(invT,D))
                    term(3)=dot_product(D,matmul(invT,D))                

!!!!!!!!!!!!!!!!!!!!CALCUL DE LA LEVEL SET AU POINT INCONNU!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                   Calcul du discriminant
                    delta = (term(2))**2.d0-term(1)*(term(3)-1)

!                   On garde la meilleure solution si delta négatif                     
                    if ( delta .lt. 0 ) then
                        if (ndim .eq. 3 ) then 
                            solution = r8gaem()  
                            call xsolveurtria(coor_nod, x, y, z, D, indmax, solution)
                            zr(jcalculs-1+upd_node) = min (zr(jcalculs-1+upd_node), &
                                                           solution ,dist(1) ,dist(2),dist(3))
                        else 
                            zr(jcalculs-1+upd_node)=min( zr(jcalculs-1+upd_node),dist(1),dist(2))
                        endif    
                        goto 1000
                    endif

!                   La plus grande racine est gardée 
                    p = (1/term(1))*(term(2)+sqrt(delta))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CALCUL FINAL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                   Test sur la direction de propagation
                    n=matmul(invT,D-p*Id)

                    if (ndim .eq. 3 ) then    
                        if (n(1) .lt. 0 .and. n(2) .lt. 0 .and. n(3) .lt. 0 ) then
                            zr(jcalculs-1+upd_node)=min( p, zr(jcalculs-1+upd_node) )
                        else
                            solution = r8gaem()
                            call xsolveurtria(coor_nod, x, y, z, D, indmax, solution)
                            zr(jcalculs-1+upd_node) = min (zr(jcalculs-1+upd_node),solution , &
                                                           dist(1) ,dist(2),dist(3))  
                        endif
                    else                            
                        if (n(1) .lt. 0 .and. n(2) .lt. 0 ) then
                            zr(jcalculs-1+upd_node)=min( p, zr(jcalculs-1+upd_node) )           
                        else   
                            zr(jcalculs-1+upd_node)=min( zr(jcalculs-1+upd_node),dist(1),dist(2))
                        endif
                    endif                    
                    
                    1000 continue     
                                   
!                   Ajoute la valeur du noeud dans le champ LS 
                    zr(jcnsls-1+zi(jnodto-1+minlo)) = zr(jcalculs-1+zi(jnodto-1+minlo))
   
                enddo        
            endif
        enddo    
        
!       Mise à faux du noeud pour ne plus le traiter
        zl(jvtemp-1+zi(jnodto-1+minlo)) = .false.               
    end do

    call jedema()
end subroutine    
    
