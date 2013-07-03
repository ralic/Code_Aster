subroutine nmevcv(sderro, fonact, nombcl)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmlecv.h"
    integer :: fonact(*)
    character(len=24) :: sderro
    character(len=4) :: nombcl
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! EVALUATION DE LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - BOUCLE SUR LES RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
!               'CALC' - CALCUL
!
!
!
!
    logical :: cveven
    integer :: ieven, zeven
    character(len=24) :: erreni, erreno, errfct
    integer :: jeeniv, jeenom, jeefct
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=9) :: neven, teven
    character(len=24) :: feven
    logical :: dv, cv, lfonc
    character(len=4) :: etabcl
    logical :: cvresi, cvnewt, cvfixe, cvinst
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    etabcl = 'CONT'
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    erreno = sderro(1:19)//'.ENOM'
    erreni = sderro(1:19)//'.ENIV'
    errfct = sderro(1:19)//'.EFCT'
    call jeveuo(errinf, 'L', jeinfo)
    call jeveuo(erreno, 'L', jeenom)
    call jeveuo(erreni, 'L', jeeniv)
    call jeveuo(errfct, 'L', jeefct)
    zeven = zi(jeinfo-1+1)
!
    call nmlecv(sderro, 'RESI', cvresi)
    call nmlecv(sderro, 'NEWT', cvnewt)
    call nmlecv(sderro, 'FIXE', cvfixe)
    call nmlecv(sderro, 'INST', cvinst)
!
! --- EVALUATION DE LA CONVERGENCE
!
    cveven = .true.
    do 10 ieven = 1, zeven
        teven = zk16(jeeniv-1+ieven)(1:9)
        neven = zk16(jeenom-1+ieven)(1:9)
        feven = zk24(jeefct-1+ieven)
        dv = .false.
        cv = .true.
        if (teven .eq. 'CONV_'//nombcl) then
            if (feven .eq. ' ') then
                lfonc = .true.
            else
                lfonc = isfonc(fonact,feven)
            endif
            if (neven(1:4) .eq. 'DIVE') then
                call nmerge(sderro, neven, dv)
                dv = dv.and.lfonc
                cv = .true.
            else if (neven(1:4).eq.'CONV') then
                call nmerge(sderro, neven, cv)
                cv = cv.and.lfonc
                dv = .false.
            endif
            cveven = cveven.and.(.not.dv).and.cv
        endif
10  end do
!
! --- RECUPERE CONVERGENCES PRECEDENTES
!
    if (nombcl .eq. 'NEWT') then
        cveven = cveven.and.cvresi
    endif
    if (nombcl .eq. 'FIXE') then
        cveven = cveven.and.cvnewt.and.cvresi
    endif
    if (nombcl .eq. 'INST') then
        cveven = cveven.and.cvfixe.and.cvnewt.and.cvresi
    endif
    if (nombcl .eq. 'CALC') then
        cveven = cveven.and.cvinst.and.cvfixe.and.cvnewt.and.cvresi
    endif
!
    if (cveven) etabcl = 'CONV'
!
! --- ENREGISTREMENT DE LA CONVERGENCE
!
    call nmeceb(sderro, nombcl, etabcl)
!
    call jedema()
end subroutine
