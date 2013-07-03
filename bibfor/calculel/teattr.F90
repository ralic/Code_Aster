subroutine teattr(typel, kstop, noattr, vattr, iret)
    implicit none
!
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
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: typel, kstop, noattr, vattr
    integer :: iret
!---------------------------------------------------------------------
! BUT : RECUPERER LA VALEUR D'UN ATTRIBUT D'UN TYPE_ELEMENT
!---------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
!     IN TYPEL  (K16) : NOM DU TYPE_ELEMENT A INTERROGER
!                        (OU ' ' SI L'ON EST "SOUS" LA ROUTINE TE0000)
!     IN NOATTR (K16) : NOM DE L'ATTRIBUT RECHERCHE
!     IN KSTOP  (K1)  : 'S' => ERREUR <F> SI ATTRIBUT ABSENT
!                     : 'C' => ON CONTINUE SI ATTRIBUT ABSENT (IRET=1)
!    OUT VATTR  (K16) : VALEUR DE L'ATTRIBUT (OU "NON_DEFINI" SI ABSENT)
!    OUT IRET   (I)   : CODE DE RETOUR :  0 -> OK
!                                         1 -> ATTRIBUT ABSENT
!-----------------------------------------------------------------------
!  CETTE ROUTINE EST ACCESSIBLE PARTOUT DANS LE CODE. SI ELLE EST
!  APPELEE EN DEHORS DE TE0000 (AVEC TYPEL != ' '), ELLE NECESSITE
!  DES APPELS JEVEUX, ELLE DEVIENT DONC UN PEU COUTEUSE.
!-----------------------------------------------------------------------
!     COMMON CALCUL :
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!  VARIABLES LOCALES :
    character(len=16) :: nomt2, noatt2, vattr2, kbid
    character(len=24) :: valk(2)
    integer :: jcte, n1, nbattr, k, ite
    logical :: apelje
!
!----------------------------------------------------------------------
    nomt2=typel
    noatt2=noattr
!
    if (nomt2 .eq. ' ') call assert(iactif.eq.1)
    apelje=.true.
    if ((iactif.eq.1) .and. (nomt2.eq.' ')) apelje=.false.
    if (nomt2 .eq. ' ') nomt2=nomte
!
!
    if (apelje) then
        call jenonu(jexnom('&CATA.TE.NOMTE', nomt2), ite)
        call jelira(jexnum('&CATA.TE.CTE_ATTR', ite), 'LONMAX', n1, kbid)
        if (n1 .gt. 0) then
            call jeveuo(jexnum('&CATA.TE.CTE_ATTR', ite), 'L', jcte)
        else
            jcte=0
        endif
    else
        jcte=jcteat
        n1=lcteat
    endif
!
    nbattr=n1/2
    do 1, k=1,nbattr
    if (zk16(jcte-1+2*(k-1)+1) .eq. noatt2) then
        vattr2=zk16(jcte-1+2*(k-1)+2)
        goto 2
    endif
    1 end do
!
    iret=1
    vattr='NON_DEFINI'
    if (kstop .eq. 'S') then
        valk(1) = noatt2
        valk(2) = nomt2
        call u2mesk('F', 'CALCULEL4_94', 2, valk)
    endif
    call assert(kstop.eq.'C')
    goto 3
!
 2  continue
    iret=0
    vattr=vattr2
!
 3  continue
!
end subroutine
