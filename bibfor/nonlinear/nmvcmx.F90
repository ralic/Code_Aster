subroutine nmvcmx(mate, mailla, comref, comval)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/r8maem.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmvcex.h"
#include "asterfort/utmess.h"
!
    character(len=24) :: mate, comref
    character(len=19) :: comval
    character(len=8) :: mailla
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! RECHERCHE DES MAXIMUM/ MIMINUM DES VARIABLES DE COMMANDES
!
! ----------------------------------------------------------------------
!
!
! IN  MATE   : CHAMP MATERIAU
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMVAL : VARI_COM
!
!
!
!
    integer :: nbcmp, nbcmp2
    character(len=8) :: valk(5)
    character(len=19) :: chsref, chscom
    character(len=24) :: vrcplu, vrcref
    integer :: jcesd, jcesl,  nbma, nbpt, nbsp, icmp
    integer :: jcrsd, jcrsl,  ima, ipt, isp, iad, iad2
    integer :: imamax, imamin,   iref
    real(kind=8) :: valmin, valmax, valr(2)
    real(kind=8) :: valeur, valref
    character(len=8), pointer :: cvrcvarc(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: crsv(:) => null()
    character(len=8), pointer :: cvrcnom(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    chscom = '&&NMVCMX.COMVAL_SIM'
    chsref = '&&NMVCMX.COMREF_SIM'
!
! --- EXTRACTION DES VARIABLES DE COMMANDE
!
    call nmvcex('TOUT', comref, vrcref)
    call nmvcex('TOUT', comval, vrcplu)
!
! --- TRANSFO. EN CHAM_NO_S
!
    call celces(vrcplu, 'V', chscom)
    call celces(vrcref, 'V', chsref)
!
    call utmess('A+', 'MECANONLINE2_97')
!
!     CALCUL DU MIN / MAX
!
!     DESCRIPTEUR
    call jeveuo(chscom//'.CESD', 'L', jcesd)
    call jeveuo(chsref//'.CESD', 'L', jcrsd)
!     PRESENCE DES CMP (R)
    call jeveuo(chscom//'.CESL', 'L', jcesl)
    call jeveuo(chsref//'.CESL', 'L', jcrsl)
!     VALEUR DES CMP (R)
    call jeveuo(chscom//'.CESV', 'L', vr=cesv)
    call jeveuo(chsref//'.CESV', 'L', vr=crsv)
!
!     RECUPERATION DES NOMS DES VARC
    call jelira(mate(1:8)//'.CVRCNOM', 'LONMAX', ival=nbcmp2)
    call jeveuo(mate(1:8)//'.CVRCNOM', 'L', vk8=cvrcnom)
    call jeveuo(mate(1:8)//'.CVRCVARC', 'L', vk8=cvrcvarc)
!
    nbma = zi(jcesd-1+1)
!
    do 10,icmp = 1,nbcmp2
    valmax=-r8maem()
    valmin=r8maem()
    imamin=0
    imamax=0
    iref=0
    if (cvrcvarc(icmp) .eq. 'TEMP' .or. cvrcvarc(icmp) .eq. 'SECH') then
        iref=1
    endif
!
    do 40,ima = 1,nbma
    nbcmp = zi(jcesd-1+5+4* (ima-1)+3)
    if (nbcmp .eq. 0) goto 40
    call cesexi('C', jcrsd, jcrsl, ima, 1,&
                1, icmp, iad2)
    if (iad2 .le. 0) goto 40
!
!
!           VALEURS DE REFERENCE
    if (iref .eq. 1) then
        call cesexi('C', jcrsd, jcrsl, ima, 1,&
                    1, icmp, iad2)
        valref = crsv(iad2)
    endif
    nbpt = zi(jcesd-1+5+4* (ima-1)+1)
    nbsp = zi(jcesd-1+5+4* (ima-1)+2)
    do 30,ipt = 1,nbpt
    do 20,isp = 1,nbsp
    call cesexi('C', jcesd, jcesl, ima, ipt,&
                isp, icmp, iad)
    if (iad .gt. 0) then
        valeur = cesv(iad)
        if (iisnan(valeur) .ne. 0) goto 20
!
        if (iref .eq. 1) then
            valeur=abs(valeur-valref)
        endif
        if (valeur .gt. valmax) then
            imamax=ima
            valmax=valeur
        endif
        if (valeur .lt. valmin) then
            imamin=ima
            valmin=valeur
        endif
    endif
20  continue
30  continue
40  continue
    if (imamax .gt. 0) then
        valk(2)=cvrcnom(icmp)
        valk(1)=cvrcvarc(icmp)
        valr(1)=valmax
        valr(2)=valmin
        call jenuno(jexnum(mailla//'.NOMMAI', imamax), valk(3))
        call jenuno(jexnum(mailla//'.NOMMAI', imamin), valk(4))
        if (iref .eq. 1) then
            valk(5)=valk(1)
            call utmess('A+', 'MECANONLINE2_95', nk=5, valk=valk, nr=2,&
                        valr=valr)
        else
            call utmess('A+', 'MECANONLINE2_94', nk=4, valk=valk, nr=2,&
                        valr=valr)
        endif
    endif
    10 end do
    call utmess('A', 'MECANONLINE2_93')
!
    call jedetr(chscom)
    call jedetr(chsref)
    call jedema()
end subroutine
