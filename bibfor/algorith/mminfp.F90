subroutine mminfp(izone, defico, questz, irep, rrep,&
                  lrep)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico
    integer :: izone
    character(len=*) :: questz
    integer :: irep(*)
    real(kind=8) :: rrep(*)
    aster_logical :: lrep(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! REPOND A UNE QUESTION SUR UNE OPTION/CARACTERISTIQUE DU CONTACT
! VARIABLE SUIVANT LA ZONE
!
! ----------------------------------------------------------------------
!
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT QU'ON INTERROGE
! IN  QUESTI : QUESTION POSEE
! OUT IREP   : VALEUR SI C'EST UN ENTIER
! OUT RREP   : VALEUR SI C'EST UN REEL
! OUT LREP   : VALEUR SI C'EST UN BOOLEEN
!
! ----------------------------------------------------------------------
!
    integer :: iform, nzoco
    integer :: zcmcf, zmeth, ztole, zexcl, zdirn, zcmdf, zcmxf
    character(len=24) :: caracf, caradf, caraxf, dirnor, methco
    integer :: jcmcf, jcmdf, jcmxf, jdirno, jmeth
    character(len=24) :: toleco, dirapp, exclfr
    integer :: jtole, jdirap, jexclf
    character(len=24) :: jeufo1, jeufo2
    integer :: jjfo1, jjfo2
    character(len=8) :: jeuf1, jeuf2
    character(len=24) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    irep(1)= 0
    rrep(1)= 0.d0
    lrep(1)= .false.
    questi = questz
!
! --- ACCES AUX SDS
!
    caracf = defico(1:16)//'.CARACF'
    caradf = defico(1:16)//'.CARADF'
    caraxf = defico(1:16)//'.CARAXF'
    dirapp = defico(1:16)//'.DIRAPP'
    dirnor = defico(1:16)//'.DIRNOR'
    methco = defico(1:16)//'.METHCO'
    toleco = defico(1:16)//'.TOLECO'
    exclfr = defico(1:16)//'.EXCLFR'
    jeufo1 = defico(1:16)//'.JFO1CO'
    jeufo2 = defico(1:16)//'.JFO2CO'
!
    zmeth = cfmmvd('ZMETH')
    ztole = cfmmvd('ZTOLE')
    zcmcf = cfmmvd('ZCMCF')
    zcmdf = cfmmvd('ZCMDF')
    zcmxf = cfmmvd('ZCMXF')
    zexcl = cfmmvd('ZEXCL')
    zdirn = cfmmvd('ZDIRN')
!
    nzoco = cfdisi(defico,'NZOCO' )
    iform = cfdisi(defico,'FORMULATION')
    ASSERT(izone.gt.0)
    if (nzoco .ne. 0) then
        ASSERT(izone.le.nzoco)
    endif
!
! ---- INTERROGATION METHCO
!
! --- APPARIEMENT
    if (questi(1:11) .eq. 'APPARIEMENT') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+1-1)
! --- DIST_POUTRE
    else if (questi.eq.'DIST_POUTRE') then
        call jeveuo(methco, 'L', jmeth)
        lrep(1) = zi(jmeth+zmeth*(izone-1)+2-1).eq.1
! --- DIST_COQUE
    else if (questi.eq.'DIST_COQUE') then
        call jeveuo(methco, 'L', jmeth)
        lrep(1) = zi(jmeth+zmeth*(izone-1)+3-1).eq.1
! --- DIST_MAIT
    else if (questi.eq.'DIST_MAIT') then
        call jeveuo(jeufo1, 'L', jjfo1)
        jeuf1 = zk8(jjfo1+izone-1)
        if (jeuf1 .eq. ' ') then
            lrep(1) = .false.
        else
            lrep(1) = .true.
        endif
! --- DIST_ESCL
    else if (questi.eq.'DIST_ESCL') then
        call jeveuo(jeufo2, 'L', jjfo2)
        jeuf2 = zk8(jjfo2+izone-1)
        if (jeuf2 .eq. ' ') then
            lrep(1) = .false.
        else
            lrep(1) = .true.
        endif
!
! --- NORMALE
    else if (questi.eq.'NORMALE') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+4-1)
! --- NORMALE = 'MAIT'
    else if (questi.eq.'MAIT') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+4-1) .eq. 0) then
            lrep(1) = .true.
        else
            lrep(1) = .false.
        endif
! --- NORMALE = 'MAIT_ESCL'
    else if (questi.eq.'MAIT_ESCL') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+4-1) .eq. 1) then
            lrep(1) = .true.
        else
            lrep(1) = .false.
        endif
! --- NORMALE = 'ESCL'
    else if (questi.eq.'ESCL') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+4-1) .eq. 2) then
            lrep(1) = .true.
        else
            lrep(1) = .false.
        endif
! --- VECT_MAIT
    else if (questi.eq.'VECT_MAIT') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+5-1)
!
    else if (questi.eq.'VECT_MAIT_DIRX') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+5-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1))
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'VECT_MAIT_DIRY') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+5-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1)+1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'VECT_MAIT_DIRZ') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+5-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1)+2)
        else
            ASSERT(.false.)
        endif
! --- VECT_ESCL
    else if (questi.eq.'VECT_ESCL') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+6-1)
!
    else if (questi.eq.'VECT_ESCL_DIRX') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+6-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1)+3)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'VECT_ESCL_DIRY') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+6-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1)+4)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'VECT_ESCL_DIRZ') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+6-1) .gt. 0) then
            call jeveuo(dirnor, 'L', jdirno)
            rrep(1) = zr(jdirno+zdirn*(izone-1)+5)
        else
            ASSERT(.false.)
        endif
! --- TYPE_APPA
    else if (questi.eq.'TYPE_APPA') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+7-1)
!
    else if (questi.eq.'TYPE_APPA_FIXE') then
        call jeveuo(methco, 'L', jmeth)
        lrep(1) = zi(jmeth+zmeth*(izone-1)+7-1).eq.1
!
    else if (questi.eq.'TYPE_APPA_DIRX') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+7-1) .eq. 1) then
            call jeveuo(dirapp, 'L', jdirap)
            rrep(1) = zr(jdirap+3*(izone-1))
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'TYPE_APPA_DIRY') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+7-1) .eq. 1) then
            call jeveuo(dirapp, 'L', jdirap)
            rrep(1) = zr(jdirap+3*(izone-1)+1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'TYPE_APPA_DIRZ') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)+7-1) .eq. 1) then
            call jeveuo(dirapp, 'L', jdirap)
            rrep(1) = zr(jdirap+3*(izone-1)+2)
        else
            ASSERT(.false.)
        endif
! --- NBMAE
    else if (questi.eq.'NBMAE') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+8 -1)
! --- NBNOE
    else if (questi.eq.'NBNOE') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+9 -1)
! --- NBMAM
    else if (questi.eq.'NBMAM') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+10-1)
! --- NBNOM
    else if (questi.eq.'NBNOM') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+11-1)
! --- NBMAET
    else if (questi.eq.'NBMAEC') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+12-1)
! --- NBNOET
    else if (questi.eq.'NBNOEC') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+13-1)
! --- NBMAM
    else if (questi.eq.'NBMAMC') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+14-1)
! --- NBNOM
    else if (questi.eq.'NBNOMC') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+15-1)
! --- JDECME
    else if (questi.eq.'JDECME') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+16-1)
! --- JDECMM
    else if (questi.eq.'JDECMM') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+17-1)
! --- JDECNE
    else if (questi.eq.'JDECNE') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+18-1)
! --- JDECNM
    else if (questi.eq.'JDECNM') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+19-1)
! --- NBPT
    else if (questi.eq.'NBPT') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+20-1)
! --- NBPC
    else if (questi.eq.'NBPC') then
        call jeveuo(methco, 'L', jmeth)
        irep(1) = zi(jmeth+zmeth*(izone-1)+21-1)
! --- VERIF
    else if (questi.eq.'VERIF') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)-1+22) .eq. 0) then
            lrep(1) = .false.
        else if (zi(jmeth+zmeth*(izone-1)-1+22).eq.1) then
            lrep(1) = .true.
        else
            ASSERT(.false.)
        endif
! --- VERIF
    else if (questi.eq.'CALCUL') then
        call jeveuo(methco, 'L', jmeth)
        if (zi(jmeth+zmeth*(izone-1)-1+22) .eq. 0) then
            lrep(1) = .true.
        else if (zi(jmeth+zmeth*(izone-1)-1+22).eq.1) then
            lrep(1) = .false.
        else
            ASSERT(.false.)
        endif
!
! ---- INTERROGATION TOLECO
!
    else if (questi.eq.'TOLE_PROJ_EXT') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+15-1)
        else
            call jeveuo(toleco, 'L', jtole)
            rrep(1) = zr(jtole+ztole*(izone-1)+1-1)
        endif
!
    else if (questi.eq.'TOLE_APPA') then
        call jeveuo(toleco, 'L', jtole)
        rrep(1) = zr(jtole+ztole*(izone-1)+2-1)
!
    else if (questi.eq.'TOLE_INTERP') then
        call jeveuo(toleco, 'L', jtole)
        rrep(1) = zr(jtole+ztole*(izone-1)+3-1)
!
! --- INTERROGATION CARACF
!
    else if (questi.eq.'ALGO_CONT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf-1+zcmcf*(izone-1)+3)
            irep(1) = nint(rrep(1))
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+11-1)
            irep(1) = nint(rrep(1))
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'ALGO_CONT_PENA') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            if (nint(zr(jcmcf-1+zcmcf*(izone-1)+3)) .eq. 3) then
                lrep(1) = .true.
            else
                lrep(1) = .false.
            endif
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+11-1)
            if (nint(rrep(1)) .eq. 2) then
                lrep(1) = .true.
            else
                lrep(1) = .false.
            endif
        endif
!
    else if (questi.eq.'ALGO_FROT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf-1+zcmcf*(izone-1)+5)
            irep(1) = nint(rrep(1))
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+13-1)
            irep(1) = nint(rrep(1))
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'ALGO_FROT_PENA') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            if (nint(zr(jcmcf-1+zcmcf*(izone-1)+5)) .eq. 3) then
                lrep(1) = .true.
            else
                lrep(1) = .false.
            endif
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1)=zr(jcmxf+zcmxf*(izone-1)+13-1)
            if (nint(rrep(1)) .eq. 2) then
                lrep(1) = .true.
            else
                lrep(1) = .false.
            endif
        endif
!
! ---- INTERROGATION CARAXF
!
    else if (questi.eq.'RELATION') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+16-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'XFEM_ALGO_LAGR') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            irep(1) = nint(zr(jcmxf+zcmxf*(izone-1)+9-1))
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'CONT_XFEM_CZM') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+11-1)
            if (nint(rrep(1)) .eq. 3) then
                lrep(1) = .true.
            else
                lrep(1) = .false.
            endif
        else
            ASSERT(.false.)
        endif
!
! ---- INTERROGATION MIXTE CARADF/CARACF/CARAXF
!
    else if (questi.eq.'GLISSIERE_ZONE') then
        lrep(1) = .false.
        if (iform .eq. 1) then
            call jeveuo(caradf, 'L', jcmdf)
            lrep(1) = nint(zr(jcmdf+zcmdf*(izone-1)+6 -1)).eq.1
        else if (iform.eq.2) then
            call jeveuo(caracf, 'L', jcmcf)
            lrep(1) = nint(zr(jcmcf-1+zcmcf*(izone-1)+9)).eq.1
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            lrep(1) = nint(zr(jcmxf+zcmxf*(izone-1)+10-1)).eq.1
        else
            ASSERT(.false.)
        endif
!
! ---- INTERROGATION MIXTE CARACF/CARAXF
!
    else if (questi.eq.'INTEGRATION') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            irep(1) = nint(zr(jcmcf-1+zcmcf*(izone-1)+1))
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            irep(1) = nint(zr(jcmxf+zcmxf*(izone-1)+1-1))
        else
            ASSERT(.false.)
        endif
!
!
    else if (questi.eq.'COEF_COULOMB') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf-1+zcmcf*(izone-1)+6)
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+4-1)
        else if (iform.eq.1) then
            call jeveuo(caradf, 'L', jcmdf)
            rrep(1) = zr(jcmdf+zcmdf*(izone-1)+4-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'COEF_AUGM_CONT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf+zcmcf*(izone-1)+2-1)
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+2-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'COEF_AUGM_FROT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf+zcmcf*(izone-1)+4-1)
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+3-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'FROTTEMENT_ZONE') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf-1+zcmcf*(izone-1)+5)
            irep(1) = nint(rrep(1))
            lrep(1) = irep(1).ne.0
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            irep(1) = nint(zr(jcmxf+zcmxf*(izone-1)+5-1))
            lrep(1) = (nint(zr(jcmxf+zcmxf*(izone-1)+5-1)).eq.3)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'SEUIL_INIT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            rrep(1) = zr(jcmcf-1+zcmcf*(izone-1)+7)
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+6-1)
        else
            ASSERT(.false.)
        endif
    else if (questi.eq.'SEUIL_AUTO') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            lrep(1) = (nint(zr(jcmcf-1+zcmcf*(izone-1)+13)) .eq. 1)
        else
            ASSERT(.false.)
        endif

!
!
    else if (questi.eq.'COEF_PENA_CONT') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+12-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'COEF_PENA_FROT') then
        if (iform .eq. 3) then
            call jeveuo(caraxf, 'L', jcmxf)
            rrep(1) = zr(jcmxf+zcmxf*(izone-1)+14-1)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'CONTACT_INIT') then
        if (iform .eq. 2) then
            call jeveuo(caracf, 'L', jcmcf)
            irep(1) = nint(zr(jcmcf-1+zcmcf*(izone-1)+8))
        else if (iform.eq.3) then
            call jeveuo(caraxf, 'L', jcmxf)
            irep(1) = nint(zr(jcmxf+zcmxf*(izone-1)+7-1))
        else
            ASSERT(.false.)
        endif
!
! ---- INTERROGATION CARADF
!
    else if (questi.eq.'COEF_MATR_FROT') then
        call jeveuo(caradf, 'L', jcmdf)
        rrep(1) = zr(jcmdf+zcmdf*(izone-1)+1-1)
!
    else if (questi.eq.'E_N') then
        call jeveuo(caradf, 'L', jcmdf)
        rrep(1) = zr(jcmdf+zcmdf*(izone-1)+2-1)
!
    else if (questi.eq.'E_T') then
        call jeveuo(caradf, 'L', jcmdf)
        rrep(1) = zr(jcmdf+zcmdf*(izone-1)+3-1)
!
    else if (questi.eq.'ALARME_JEU') then
        call jeveuo(caradf, 'L', jcmdf)
        rrep(1) = zr(jcmdf+zcmdf*(izone-1)+5-1)
!
! --- INTERROGATION EXCLFR
!
    else if (questi.eq.'EXCL_DIR') then
        call jeveuo(caracf, 'L', jcmcf)
        irep(1) = nint(zr(jcmcf-1+zcmcf*(izone-1)+12))
!
    else if (questi.eq.'EXCL_FROT_DIRX') then
        call jeveuo(exclfr, 'L', jexclf)
        rrep(1) = zr(jexclf-1+zexcl*(izone-1)+1)
    else if (questi.eq.'EXCL_FROT_DIRY') then
        call jeveuo(exclfr, 'L', jexclf)
        rrep(1) = zr(jexclf-1+zexcl*(izone-1)+2)
    else if (questi.eq.'EXCL_FROT_DIRZ') then
        call jeveuo(exclfr, 'L', jexclf)
        rrep(1) = zr(jexclf-1+zexcl*(izone-1)+3)
!
    else
        write(6,*) '   NUM. ZONE    : <',izone  ,'>'
        write(6,*) '   QUESTION     : <',questi ,'>'
        write(6,*) '   REPONSE  - I : <',irep(1),'>'
        write(6,*) '   REPONSE  - R : <',rrep(1),'>'
        write(6,*) '   REPONSE  - L : <',lrep(1),'>'
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
