subroutine tecach(stopz, nmparz, louez, iret, nval,&
                  itab, iad, numa)
use calcul_module, only : ca_evfini_, ca_iachoi_, ca_iachok_, ca_iaoppa_,&
     ca_iawlo2_, ca_iawloc_, ca_iawtyp_, ca_iel_,&
     ca_igr_, ca_jrepe_, ca_nbgr_, ca_nomte_, ca_nparin_, ca_npario_, ca_option_
implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
!----------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
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
!     BUT:
!     ---
!     OBTENIR DES INFORMATIONS SUR LE CHAMP LOCAL ASSOCIE A UN
!     PARAMETRE DANS UNE ROUTINE TE00IJ (OU EN DESSOUS)
!
!
!     ENTREES:
!     --------
! NUMA    : / 0  : ELEMENT COURANT
!           / >0 : NUMERO D'1 MAILLE DU MAILLAGE
! NMPARZ  : NOM DU PARAMETRE DE L'OPTION
! LOUEZ   : 'L' OU 'E'  ( LECTURE/ECRITURE )
! NVAL    : NOMBRE DE VALEURS DESIREES DANS ITAB(*)
! STOPZ   : PERMET DE DIRE A TECAC2 DE S'ARRETER SI ...
!
! 1) STOPZ(1:1) : SI LE PARAMETRE N'APPARAIT PAS DANS LE CATALOGUE
!                 DU TYPE_ELEMENT (OU CELUI DE L'OPTION)
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=1 , ITAB(1)=0
!
! 2) STOPZ(2:2) : SI  LE CHAMP_LOCAL ASSOCIE AU PARAMETRE N'EXISTE PAS
!        CE QUI PEUT ARRIVER POUR 2 RAISONS :
!         2-1) LE PARAMETRE N'APPARTIENT PAS A LPAIN (OU LPAOUT)
!         2-2) LE CHAMP GLOBAL (CHIN) ASSOCIE AU PARAMETRE N'EXISTE PAS
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=2 , ITAB(1)=0
!
! 3) STOPZ(3:3) : SI  LE CHAMP_LOCAL ASSOCIE AU PARAMETRE EST INCOMPLET
!        (I.E. ON N'A PAS PU EXTRAIRE TOUTES LES CMPS VOULUES)
!            'O'  -> ON S'ARRETE EN ERREUR <F>
!            'N'  -> ON NE S'ARRETE PAS
!                    => IRET=3,ITAB(1)=ADRESSE DU CHAMP LOCAL INCOMPLET
!                       POUR S'EN SORTIR IL FAUT UTILISER ITAB(8)
!                       REMARQUE : SI NVAL < 8, ON REND ITAB(1)=0 POUR
!                         EVITER D'UTILISER UNE ADRESSE INUTILISABLE
!
!
!
!
!
! SORTIES:
! --------
! IRET : CODE RETOUR :
!       0 -> TOUT OK
!       1 -> LE PARAMETRE N'EXISTE PAS DANS LE CATALOGUE DE L'ELEMENT
!       2 -> LE CHAMP N'EST PAS FOURNI PAR L'APPELANT DE CALCUL
!       3 -> LE CHAMP EST INCOMPLET : IL MANQUE DES CMPS
!
! ITAB(1)   : ADRESSE DU CHAMP_LOCAL (DANS ZR, ZC, ....)
!             = 0  SI IL N'EXISTE PAS DE CHAMP LOCAL (IRET=1,2)
!
!
! ITAB(2)   : LONGUEUR DU CHAMP_LOCAL DANS LE CATALOGUE
!             (NE TIENT PAS COMPTE DE NCDYN ET NBSPT
!              VOIR CI-DESSOUS ITAB(6) ET ITAB(7) )
! ITAB(3)   : NOMBRE DE POINTS DE LOCALISATION DU CHAMP
! ITAB(4)   : 9999 (INUTILISE)
! ITAB(5)   : TYPE_SCALAIRE DU CHAMP :
!             1 --> REEL
!             2 --> COMPLEXE
!             3 --> ENTIER
!             4 --> K8
!             5 --> K16
!             6 --> K24
! ITAB(6)   : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R
! ITAB(7)   : NBSPT : NOMBRE DE SOUS-POINTS
! ITAB(8)   : ADRESSE (DANS ZL) D'UN VECTEUR DE BOOLEENS
!             PARALLELE AU CHAMP LOCAL PERMETTANT DE SAVOIR QUELLES
!             SONT LES CMPS PRESENTES ET ABSENTES
!
!     -----------------------------------------------------------------
    character(len=8) :: nompar, stop8
    character(len=1) :: loue
    aster_logical :: exichl, etendu
    integer ::  inuma, inval, jtab(8)
    integer ::   iparg
    integer ::  jceld,  adiel, debugr, nbspt, ncdyn
    integer :: lgcata,  decael, lonchl, iachlo, ilchlo
    integer :: k, jrsvi, jcrsvi, i1, ich, iel2, igr2, debgr2
!
!
    character(len=24) :: valk(3)
    aster_logical :: stpcat, stpexi, stpinc
!
!   DEB--------------------------------------------------------------
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
!
    nompar = nmparz
    stop8 = stopz
    loue = louez
!
    stpcat = (stop8(1:1).eq.'O')
    stpexi = (stop8(2:2).eq.'O')
    stpinc = (stop8(3:3).eq.'O')
!
    ASSERT(loue.eq.'L' .or. loue.eq.'E')
    ASSERT(1.le.inval .and. inval.le.8)
    iret = 0
    jtab(1) = 0
!
!
!     1- SI LE PARAMETRE N'APPARTIENT PAS A L'OPTION :
!     -------------------------------------------------
    exichl = .false.
!
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
!
!
!     2- SI LE PARAMETRE APPARTIENT A L'OPTION :
!     -------------------------------------------------
!
! --- ON VERIFIE QUE LES PARAMETRE IN SONT EN LECTURE
!     ET QUE LES PARAMETRES OUT SONT EN ECRITURE
    if (iparg .gt. ca_nparin_ .and. loue .eq. 'L') then
        write(6,*)'PARAMETRE OUT EN LECTURE : ',nompar
        ASSERT(.false.)
    else if (iparg.le.ca_nparin_ .and. loue.eq.'E') then
        write(6,*)'PARAMETRE IN EN ECRITURE : ',nompar
        ASSERT(.false.)
    endif
!
    iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
    ilchlo=zi(ca_iawloc_-1+3*(iparg-1)+2)
    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+2)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+5)
!
!     -- CALCUL DE JRSVI ET JCRSVI :
    jrsvi=0
    if (ca_evfini_ .eq. 1) then
        ich=zi(ca_iawloc_-1+3*(iparg-1)+3)
        if (iparg .gt. ca_nparin_ .and. ich .gt. 0) then
            if (zk8(ca_iachok_-1+2*(ich-1)+1) .eq. 'RESL') then
                jrsvi=zi(ca_iachoi_-1+3*(ich-1)+2)
                jcrsvi=zi(ca_iachoi_-1+3*(ich-1)+3)
            endif
        endif
    endif
!
    if (iachlo .eq. -1) iret = 2
    if (lgcata .eq. -1) iret = 1
!
!
!     -- SI IACHLO=-1    : LE CHAMP N'EXISTE PAS (GLOBALEMENT)
!     -- SI LGCATA=-1 : LE PARAMETRE N'EXISTE PAS POUR LE TYPE_ELEMENT
!     -------------------------------------------------
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
!
        endif
!
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
!
!
!     -------------------------------------------------
!
    if (.not.exichl) then
        goto 20
    endif
!
!
!     ITAB(1) : ADRESSE DU CHAMP LOCAL POUR L'ELEMENT IEL2 :
!     -----------------------------------------------------
!
!     -- CALCUL DE ITAB(1),LONCHL,DECAEL,NBSPT,NCDYN :
!     -------------------------------------------------
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
        if (jrsvi .eq. 0) then
            decael = (iel2-1)*lgcata
            lonchl = lgcata
        else
            i1 = zi(jcrsvi-1+igr2)
            decael = zi(jrsvi-1+i1-1+iel2)
            lonchl = zi(jrsvi-1+i1-1+iel2+1) - decael
            decael = decael -1
        endif
    endif
    jtab(1) = iachlo+debugr-1+decael
!
!
!     -- POUR LES CHAMPS "IN" ON VERIFIE QUE L'EXTRACTION EST
!        COMPLETE SUR L'ELEMENT:
!     ----------------------------------------------------------
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
!
    if (inval .lt. 2) goto 20
!
!
!     ITAB(2) : LONGUEUR DU CHAMP LOCAL (CATALOGUE) :
!     -----------------------------------------------------
    jtab(2) = lgcata
    if (inval .lt. 3) goto 20
!
!
!     ITAB(3) : NOMBRE DE POINTS (CATALOGUE) :
!     -----------------------------------------------------
    jtab(3)=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+3)
    if (inval .lt. 4) goto 20
    jtab(4) = 9999
    if (inval .lt. 5) goto 20
!
!
!     ITAB(5) : TYPE DU CHAMP LOCAL  :
!           R/C/I/K8/K16/K24
!           1/2/3/4 /5  /6
!     -----------------------------------------------------
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
!
!
!     ITAB(6) : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R (IEL2)
!     -------------------------------------------------------
    jtab(6) = ncdyn
    if (inval .lt. 7) goto 20
!
!
!     ITAB(7) : NBSPT : NOMBRE DE SOUS-POINTS POUR IEL2
!     -----------------------------------------------------
    jtab(7) = nbspt
    if (inval .lt. 8) goto 20
!
!
!     ITAB(8) : ADRESSE DU VECTEUR DE BOOLEENS :
!     -----------------------------------------------------
    jtab(8) = ilchlo+debugr-1+decael
    if (inval .lt. 9) goto 20
!
 20 continue
    if (present(iad)) then
        iad = jtab(1)
    else
        do k = 1, inval
            itab(k) = jtab(k)
        end do
    endif
!
end subroutine
