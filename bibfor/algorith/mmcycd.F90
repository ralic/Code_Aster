subroutine mmcycd(noma, defico, resoco)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mmcyc1.h'
    include 'asterfort/mmcyc2.h'
    include 'asterfort/mmcyc3.h'
    include 'asterfort/mmcyc4.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfm.h'
    include 'asterfort/mmnpoi.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! DETECTION DES CYCLAGES
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: ztabf
    integer :: indco, indfr
    real(kind=8) :: rese(3)
    integer :: nbmae, nptm
    logical :: lveri
    integer :: nzoco
    logical :: lfrot, lboucc
    integer :: izone, iptm, imae, iptc
    integer :: jdecme, posmae, nummae
    character(len=8) :: nommae
    character(len=16) :: nompt
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... CYCLAGE'
    endif
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    nompt = ' '
    nzoco = cfdisi(defico,'NZOCO')
    lfrot = cfdisl(defico,'FROTTEMENT')
    lboucc = cfdisl(defico,'CONT_BOUCLE')
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do 10 izone = 1, nzoco
!
! ----- OPTIONS SUR LA ZONE DE CONTACT
!
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        if (lveri) goto 25
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- STATUTS
!
                indco = nint(zr(jtabf+ztabf*(iptc-1)+22))
                indfr = nint(zr(jtabf+ztabf*(iptc-1)+23))
                rese(1) = zr(jtabf+ztabf*(iptc-1)+25)
                rese(2) = zr(jtabf+ztabf*(iptc-1)+26)
                rese(3) = zr(jtabf+ztabf*(iptc-1)+27)
!
! --------- NOM DU POINT DE CONTACT
!
                nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
                call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
                call mmnpoi(noma, nommae, -1, iptm, nompt)
!
! --------- DETECTION DU CYCLE DE TYPE CONTACT/PAS CONTACT
!
                call mmcyc1(resoco, iptc, nompt, indco)
!
! --------- DETECTION DU CYCLE DE TYPE ADHERENT/GLISSANT
!
                if (lfrot) call mmcyc2(resoco, iptc, nompt, indco, indfr)
!
! --------- DETECTION DU CYCLE DE TYPE GLISSANT AVANT/ARRIERE
!
                if (lfrot) call mmcyc3(resoco, iptc, nompt, indco, indfr,&
                                       rese)
!
! --------- DETECTION DU CYCLE DE TYPE FLIP-FLOP HISTORIQUE
!
                if (lboucc) call mmcyc4(resoco, iptc, nompt, indco)
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
30          continue
20      continue
25      continue
10  end do
!
    call jedema()
end subroutine
