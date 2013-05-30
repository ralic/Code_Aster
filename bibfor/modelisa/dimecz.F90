subroutine dimecz(char, noma, nzoco, iform)
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfnbsf.h'
    include 'asterfort/cfnumm.h'
    include 'asterfort/cfzone.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mmelin.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    character(len=8) :: char, noma
    integer :: nzoco, iform
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! CONSTRUCTION DU VECTEUR D'INFORMATION SUR LES LONGUEURS PAR ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  IFORM  : FORMULATION DISCRETE(1) OU CONTINUE(2)
!
!
!
!
    character(len=24) :: methco
    integer :: jmeth
    integer :: zmeth
    integer :: izone, imae
    integer :: jdecme, jdecmm, jdecne, jdecnm
    integer :: isumae, isumam
    integer :: posmae, nummae
    integer :: nbnom, nbnoe, nbmam, nbmae
    integer :: nbnomc, nbnoec, nbmamc, nbmaec
    integer :: nbpt, nnint
    character(len=24) :: defico
    integer :: typint
    logical :: lveri
    integer :: nbpc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nbmae = 0
    nbnoe = 0
    nbmam = 0
    nbmae = 0
    nbpt = 0
    nbpc = 0
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    methco = defico(1:16)//'.METHCO'
    call jeveuo(methco, 'E', jmeth)
    zmeth = cfmmvd('ZMETH')
!
! --- NOMBRE DE NOEUDS ET DE MAILLES (TOTAL) PAR ZONE
! --- DECALAGES PAR ZONE
!
    do 30 izone = 1, nzoco
        call cfzone(defico, izone, 'ESCL', isumae)
        call cfnbsf(defico, isumae, 'MAIL', nbmae, jdecme)
        call cfnbsf(defico, isumae, 'NOEU', nbnoe, jdecne)
        call cfzone(defico, izone, 'MAIT', isumam)
        call cfnbsf(defico, isumam, 'MAIL', nbmam, jdecmm)
        call cfnbsf(defico, isumam, 'NOEU', nbnom, jdecnm)
!
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            nbmaec = 0
            nbmamc = 0
            nbnoec = 0
            nbnomc = 0
        else
            nbmaec = nbmae
            nbmamc = nbmam
            nbnoec = nbnoe
            nbnomc = nbnom
        endif
!
        zi(jmeth+zmeth*(izone-1)+8 -1) = nbmae
        zi(jmeth+zmeth*(izone-1)+9 -1) = nbnoe
        zi(jmeth+zmeth*(izone-1)+10-1) = nbmam
        zi(jmeth+zmeth*(izone-1)+11-1) = nbnom
        zi(jmeth+zmeth*(izone-1)+12-1) = nbmaec
        zi(jmeth+zmeth*(izone-1)+13-1) = nbnoec
        zi(jmeth+zmeth*(izone-1)+14-1) = nbmamc
        zi(jmeth+zmeth*(izone-1)+15-1) = nbnomc
30  end do
!
! --- DECALAGES PAR ZONE
!
    do 31 izone = 1, nzoco
        call cfzone(defico, izone, 'ESCL', isumae)
        call cfnbsf(defico, isumae, 'MAIL', nbmae, jdecme)
        call cfnbsf(defico, isumae, 'NOEU', nbnoe, jdecne)
        call cfzone(defico, izone, 'MAIT', isumam)
        call cfnbsf(defico, isumam, 'MAIL', nbmam, jdecmm)
        call cfnbsf(defico, isumam, 'NOEU', nbnom, jdecnm)
        zi(jmeth+zmeth*(izone-1)+16-1) = jdecme
        zi(jmeth+zmeth*(izone-1)+17-1) = jdecmm
        zi(jmeth+zmeth*(izone-1)+18-1) = jdecne
        zi(jmeth+zmeth*(izone-1)+19-1) = jdecnm
31  end do
!
! --- NOMBRE DE POINTS DE CONTACT PAR ZONE
!
    do 40 izone = 1, nzoco
        if (iform .eq. 1) then
            nbpt = mminfi(defico,'NBNOE' ,izone )
        else if (iform.eq.2) then
            nbmae = mminfi(defico,'NBMAE' ,izone )
            jdecme = mminfi(defico,'JDECME' ,izone )
            typint = mminfi(defico,'INTEGRATION',izone )
            nbpt = 0
            do 45 imae = 1, nbmae
                posmae = imae + jdecme
                call cfnumm(defico, 1, posmae, nummae)
                call mmelin(noma, nummae, typint, nnint)
                nbpt = nbpt + nnint
45          continue
        else
            call assert(.false.)
        endif
!
        zi(jmeth+zmeth*(izone-1)+20-1) = nbpt
!
! --- CONTACT EFFECTIF
!
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            nbpc = 0
        else
            nbpc = nbpt
        endif
!
        zi(jmeth+zmeth*(izone-1)+21-1) = nbpc
!
40  end do
!
    call jedema()
end subroutine
