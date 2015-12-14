subroutine tecach(stopz, nmparz, louez, iret, nval,&
                  itab, iad, numa)

use calcul_module, only : ca_iachoi_, ca_iachok_, ca_iaoppa_,&
     ca_iawlo2_, ca_iawloc_, ca_iawtyp_, ca_iel_,&
     ca_igr_, ca_jrepe_, ca_nbgr_, ca_nomte_, ca_nparin_, ca_npario_, ca_option_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/chloet.h"
#include "asterfort/contex_param.h"
#include "asterfort/utmess.h"

    character(len=*), intent(in) :: stopz, nmparz, louez
    integer, intent(in), optional :: numa
    integer, intent(in), optional :: nval
    integer, intent(out), optional :: itab(*), iad
    integer, intent(out) :: iret
!----------------------------------------------------------------------
!
! But:
! ---
! Obtenir des informations sur le champ local associe a un
! parametre dans une routine te00ij (ou en dessous)
!
! Entrees:
! --------
! numa    : /  0 : element courant
!           / >0 : numero d'1 maille du maillage
! nmparz  : nom du parametre de l'option
! louez   : 'L' ou 'E'  ( lecture/ecriture )
! nval    : nombre de valeurs desirees dans itab(*)
! stopz   : permet de dire a tecac2 de s'arreter si ...

! 1) stopz(1:1) : si le parametre n'apparait pas dans le catalogue
!                 du type_element (ou celui de l'option)
!            'O'  -> on s'arrete en erreur <f>
!            'N'  -> on ne s'arrete pas
!                    => iret=1 , itab(1)=0
!
! 2) stopz(2:2) : si  le champ_local associe au parametre n'existe pas
!        ce qui peut arriver pour 2 raisons :
!         2-1) le parametre n'appartient pas a lpain (ou lpaout)
!         2-2) le champ global (chin) associe au parametre n'existe pas
!            'O'  -> on s'arrete en erreur <f>
!            'N'  -> on ne s'arrete pas
!                    => iret=2 , itab(1)=0
!
! 3) stopz(3:3) : si  le champ_local associe au parametre est incomplet
!        (i.e. on n'a pas pu extraire toutes les cmps voulues)
!            'O'  -> on s'arrete en erreur <f>
!            'N'  -> on ne s'arrete pas
!                    => iret=3,itab(1)=adresse du champ local incomplet
!                       pour s'en sortir il faut utiliser itab(8)
!                       remarque : si nval < 8, on rend itab(1)=0 pour
!                         eviter d'utiliser une adresse inutilisable
!
! Sorties:
! --------
! iret : code retour :
!       0 -> tout ok
!       1 -> le parametre n'existe pas dans le catalogue de l'element
!       2 -> le champ n'est pas fourni par l'appelant de calcul
!       3 -> le champ est incomplet : il manque des cmps
!
! itab(1)   : adresse du champ_local (dans zr, zc, ....)
!             = 0  si il n'existe pas de champ local (iret=1,2)
!
!
! itab(2)   : longueur du champ_local dans le catalogue
!             (ne tient pas compte de ncdyn et nbspt
!              voir ci-dessous itab(6) et itab(7) )
! itab(3)   : nombre de points de localisation du champ
! itab(4)   : 9999 (inutilise)
! itab(5)   : type_scalaire du champ :
!             1 --> reel
!             2 --> complexe
!             3 --> entier
!             4 --> k8
!             5 --> k16
!             6 --> k24
! itab(6)   : ncdyn : nombre de cmp pour la grandeur vari_r
! itab(7)   : nbspt : nombre de sous-points
! itab(8)   : adresse (dans zl) d'un vecteur de booleens
!             parallele au champ local permettant de savoir quelles
!             sont les cmps presentes et absentes
!
!---------------------------------------------------------------------
    character(len=8) :: nompar, stop8
    character(len=1) :: loue
    aster_logical :: exichl, etendu
    integer ::  inuma, inval, jtab(8)
    integer ::   iparg
    integer ::  jceld,  adiel, debugr, nbspt, ncdyn
    integer :: lgcata,  decael, lonchl, iachlo, ilchlo
    integer :: k, iel2, igr2, debgr2
    character(len=24) :: valk(3)
    aster_logical :: stpcat, stpexi, stpinc
!-------------------------------------------------------------------
    if (present(numa)) then
        inuma = numa
        ASSERT(inuma.ge.0)
    else
        inuma = 0
    endif
    if (present(nval)) then
        inval = nval
        ASSERT(.not. present(iad))
        ASSERT(present(itab))
    else
        inval = 1
        ASSERT(present(iad))
    endif
    if (inuma .eq. 0) then
        igr2=ca_igr_
        iel2=ca_iel_
    else
        igr2=zi(ca_jrepe_-1+2*(inuma-1)+1)
        iel2=zi(ca_jrepe_-1+2*(inuma-1)+2)
    endif

    nompar = nmparz
    stop8 = stopz
    loue = louez

    stpcat = (stop8(1:1).eq.'O')
    stpexi = (stop8(2:2).eq.'O')
    stpinc = (stop8(3:3).eq.'O')

    ASSERT(loue.eq.'L' .or. loue.eq.'E')
    ASSERT(1.le.inval .and. inval.le.8)
    iret = 0
    jtab(1) = 0


!   1- si le parametre n'appartient pas a l'option :
!   -------------------------------------------------
    exichl = .false.

    iparg = indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
    if (iparg .eq. 0) then
        if (stpcat) then
            valk(1) = nompar
            valk(2) = ca_option_
            call utmess('E', 'CALCUL_15', nk=2, valk=valk)
            call contex_param(ca_option_, ' ')
        endif
        iret = 1
        goto 20
    endif


!   2- si le parametre appartient a l'option :
!   -------------------------------------------------

!   -- on verifie que les parametre in sont en lecture
!      et que les parametres out sont en ecriture
    if (iparg .gt. ca_nparin_ .and. loue .eq. 'L') then
        write(6,*)'PARAMETRE OUT EN LECTURE : ',nompar
        ASSERT(.false.)
    else if (iparg.le.ca_nparin_ .and. loue.eq.'E') then
        write(6,*)'PARAMETRE IN EN ECRITURE : ',nompar
        ASSERT(.false.)
    endif

    iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
    ilchlo=zi(ca_iawloc_-1+3*(iparg-1)+2)
    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+2)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+5)


    if (iachlo .eq. -1) iret = 2
    if (lgcata .eq. -1) iret = 1


!   -- si iachlo=-1    : le champ n'existe pas (globalement)
!   -- si lgcata=-1 : le parametre n'existe pas pour le type_element
!   -------------------------------------------------
    if (iachlo .eq. -1) then
        if (stpexi) then
            valk(1) = nompar
            valk(2) = ca_option_
            valk(3) = ca_nomte_
            if (nompar(1:5) .eq. 'PVARC') then
                call utmess('F', 'CALCUL_24', nk=3, valk=valk)
            else
                call utmess('E', 'CALCUL_29', nk=3, valk=valk)
                call contex_param(ca_option_, nompar)
            endif

        endif

        if (lgcata .eq. -1) then
            if (stpcat) then
                valk(1) = nompar
                valk(2) = ca_option_
                valk(3) = ca_nomte_
                call utmess('E', 'CALCUL_16', nk=3, valk=valk)
                call contex_param(ca_option_, nompar)
            endif
        endif
    else
        if (lgcata .eq. -1) then
            if (stpcat) then
                valk(1) = nompar
                valk(2) = ca_option_
                valk(3) = ca_nomte_
                call utmess('E', 'CALCUL_16', nk=3, valk=valk)
                call contex_param(ca_option_, nompar)
            endif
        else
            exichl = .true.
        endif
    endif

    if (.not.exichl) then
        goto 20
    endif


!   itab(1) : adresse du champ local pour l'element iel2 :
!   -----------------------------------------------------

!   -- calcul de itab(1),lonchl,decael,nbspt,ncdyn :
!   -------------------------------------------------
    call chloet(iparg, etendu, jceld)
    if (etendu) then
        adiel = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+4)
        debgr2 = zi(jceld-1+zi(jceld-1+4+igr2)+8)
        nbspt = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+1)
        ncdyn = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+2)
        ASSERT(lgcata.eq.zi(jceld-1+zi(jceld-1+4+igr2)+3))
        decael = (adiel-debgr2)
        lonchl = zi(jceld-1+zi(jceld-1+4+igr2)+4+4* (iel2-1)+3)
    else
        ncdyn = 0
        nbspt = 1
        decael = (iel2-1)*lgcata
        lonchl = lgcata
    endif
    jtab(1) = iachlo+debugr-1+decael


!   -- pour les champs "in" on verifie que l'extraction est
!      complete sur l'element:
!   ----------------------------------------------------------
    if (ilchlo .ne. -1) then
        do 10 k = 1, lonchl
            if (.not.zl(ilchlo+debugr-1+decael-1+k)) then
                if (stpinc) then
                    valk(1) = nompar
                    valk(2) = ca_option_
                    valk(3) = ca_nomte_
                    call utmess('E', 'CALCUL_30', nk=3, valk=valk)
                    call contex_param(ca_option_, nompar)
                else
                    iret = 3
                    jtab(1)=0
                endif
            endif
 10     continue
    endif

    if (inval .lt. 2) goto 20


!   itab(2) : longueur du champ local (catalogue) :
!   ----------------------------------------------
    jtab(2) = lgcata
    if (inval .lt. 3) goto 20


!   itab(3) : nombre de points (catalogue) :
!   ----------------------------------------
    jtab(3)=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+3)
    if (inval .lt. 4) goto 20
    jtab(4) = 9999
    if (inval .lt. 5) goto 20


!   itab(5) : type du champ local  :
!           R/C/I/K8/K16/K24
!           1/2/3/4 /5  /6
!   ----------------------------------
    if (zk8(ca_iawtyp_-1+iparg) (1:1) .eq. 'R') then
        jtab(5) = 1
    else if (zk8(ca_iawtyp_-1+iparg) (1:1).eq.'C') then
        jtab(5) = 2
    else if (zk8(ca_iawtyp_-1+iparg) (1:1).eq.'I') then
        jtab(5) = 3
    else if (zk8(ca_iawtyp_-1+iparg) (1:3).eq.'K8 ') then
        jtab(5) = 4
    else if (zk8(ca_iawtyp_-1+iparg) (1:3).eq.'K16') then
        jtab(5) = 5
    else if (zk8(ca_iawtyp_-1+iparg) (1:3).eq.'K24') then
        jtab(5) = 6
    else
        ASSERT(.false.)
    endif
    if (inval .lt. 6) goto 20


!   itab(6) : ncdyn : nombre de cmp pour la grandeur vari_r
!   -------------------------------------------------------
    jtab(6) = ncdyn
    if (inval .lt. 7) goto 20


!   itab(7) : nbspt : nombre de sous-points
!   ---------------------------------------
    jtab(7) = nbspt
    if (inval .lt. 8) goto 20


!   itab(8) : adresse du vecteur de booleens :
!   ------------------------------------------
    jtab(8) = ilchlo+debugr-1+decael
    if (inval .lt. 9) goto 20

 20 continue
    if (present(iad)) then
        iad = jtab(1)
    else
        do k = 1, inval
            itab(k) = jtab(k)
        end do
    endif

end subroutine
