subroutine teattr(kstop, noattr, vattr, iret, typel)
    implicit none
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=1), intent(in) :: kstop
    character(len=*), intent(in) :: noattr
    character(len=*), intent(out) :: vattr
    integer, intent(out) :: iret
    character(len=*), intent(in), optional :: typel
!
!---------------------------------------------------------------------
! but : Recuperer la valeur d'un attribut d'un type_element
!---------------------------------------------------------------------
!  Arguments:
!  ----------
! (o) in  kstop  (k1)  : 'S' => erreur <f> si attribut absent
!                      : 'C' => on continue si attribut absent (iret=1)
! (o) in  noattr (k16) : nom de l'attribut recherche
! (o) out vattr  (k16) : valeur de l'attribut (ou "non_defini" si absent)
! (o) out iret   (i)   : code de retour :  0 -> ok
!                                          1 -> attribut absent
! (f) in  typel  (k16) : Nom du type_element a interroger.
!                        Cet argument est inutile si la question concerne le
!                        type_element "courant".
!-----------------------------------------------------------------------
!  Cette routine est utilisable partout dans le code.
!  Si elle est appelee en dehors de te0000 il faut fournir typel.
!  Sinon, typel est inutile.
!-----------------------------------------------------------------------
! Remarque :
!  Il est aussi possible de demander les attributs :
!  *  'CODPHE' (= ALIAS8(1:2))
!  *  'CODMOD' (= ALIAS8(3:5))
!  *  'CODTMA' (= ALIAS8(6:8))
!-----------------------------------------------------------------------
!   COMMON(s) CALCUL :
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
    character(len=16) :: option, nomte, nomtm
    common /cakk01/option,nomte,nomtm
!
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!-----------------------------------------------------------------------
!   VARIABLES LOCALES :
    character(len=16) :: nomt2, noatt2, vattr2
    character(len=24) :: valk(2)
    integer :: jcte, n1, nbattr, k, ite
    aster_logical :: apelje
!
!----------------------------------------------------------------------
    if (present(typel)) then
        nomt2=typel
        ASSERT(nomt2 .ne. ' ')
    else
        nomt2=' '
    endif
!
    noatt2=noattr
!
    if (noattr .eq. 'CODPHE') noatt2='ALIAS8'
    if (noattr .eq. 'CODMOD') noatt2='ALIAS8'
    if (noattr .eq. 'CODTMA') noatt2='ALIAS8'
!
    if (nomt2 .eq. ' ') then
        ASSERT(iactif.eq.1)
        nomt2=nomte
    endif
!
    apelje=.true.
    if (iactif .eq. 1) then
        if (nomt2 .eq. nomte) apelje=.false.
    endif
!
!
    if (apelje) then
        call jenonu(jexnom('&CATA.TE.NOMTE', nomt2), ite)
        call jelira(jexnum('&CATA.TE.CTE_ATTR', ite), 'LONMAX', n1)
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
    do 1 k = 1, nbattr
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
        call utmess('F', 'CALCULEL4_94', nk=2, valk=valk)
    endif
    ASSERT(kstop.eq.'C')
    goto 3
!
  2 continue
    iret=0
    vattr=vattr2
!
    if (noattr .eq. 'CODPHE') vattr=vattr2(1:2)
    if (noattr .eq. 'CODMOD') vattr=vattr2(3:5)
    if (noattr .eq. 'CODTMA') vattr=vattr2(6:8)
!
  3 continue
!
end subroutine
