subroutine mmimp1(ifm, noma, defico, resoco)
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
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfm.h'
    include 'asterfort/mmnorm.h'
    integer :: ifm
    character(len=8) :: noma
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
!
! AFFICHAGE APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    integer :: ztabf
    integer :: posmae, nummae, nummam, numnoe, jdecme
    character(len=8) :: nommae, nommam, nomnoe
    real(kind=8) :: ksipc1, ksipc2, ksipr1, ksipr2, wpc, r8bid, seuili
    integer :: xs
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: iptm, izone, imae, inoe, iptc
    integer :: ndimg, nzoco, nnoe, nptm, nbmae
    integer :: iacnx1, ilcnx1
    logical :: lveri
!
! ----------------------------------------------------------------------
!
    call jemarq()
    write (ifm,*) '<CONTACT> ... RESULTAT DE L''APPARIEMENT'
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM' )
    nzoco = cfdisi(defico,'NZOCO')
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- ACCES MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do 10 izone = 1, nzoco
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF' ,izone )
        if (lveri) then
            goto 25
        endif
!
! ----- INFORMATION SUR LA ZONE
!
        jdecme = mminfi(defico,'JDECME',izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS DE CONTACT
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- REPERAGE MAILLE ESCLAVE
!
            nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
            call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
            nnoe = zi(ilcnx1+nummae) - zi(ilcnx1-1+nummae)
!
! ------- INFOS SUR MAILLE ESCLAVE
!
            write (ifm,1000) nommae,izone,nnoe,nptm
            1000    format (' <CONTACT>     * MAILLE ESCLAVE ',a8,' ( ZONE ',&
     &           i5,') - (',&
     &           i5,' NOEUDS ) - (',&
     &           i5,' POINTS DE CONTACT )' )
            do 21 inoe = 1, nnoe
                numnoe = zi(iacnx1+zi(ilcnx1-1+nummae)-2+inoe)
                call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
                write (ifm,1001) nomnoe
21          continue
            1001    format (' <CONTACT>        NOEUD :',a8)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- POINT DE CONTACT EN COURS
!
                write(ifm,2000) iptm
                ksipc1 = zr(jtabf+ztabf*(iptc-1)+3)
                ksipc2 = zr(jtabf+ztabf*(iptc-1)+4)
                wpc = zr(jtabf+ztabf*(iptc-1)+14)
                write(ifm,3000) ksipc1,ksipc2,wpc
                2000 format (' <CONTACT>     ** POINT DE CONTACT ',i3)
                3000 format (' <CONTACT>        SITUE EN  : <',&
     &         e10.3,',',e10.3,'> - POIDS INTEGRATION: ',e10.3)
!
! --------- REPERAGE MAILLE MAITRE
!
                nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
                call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
!
! --------- ETAT DU NOEUD
!
                ksipr1 = zr(jtabf+ztabf*(iptc-1)+5)
                ksipr2 = zr(jtabf+ztabf*(iptc-1)+6)
                if (zr(jtabf+ztabf*(iptc-1)+18) .eq. 1.d0) then
                    write(ifm,*) '<CONTACT>        EXCLUS CONTACT    '
                else if (zr(jtabf+ztabf*(iptc-1)+19).ge.1.d0) then
                    write(ifm,*) '<CONTACT>        EXCLUS FROTTEMENT '
                else
                    write(ifm,2003) nommam,ksipr1,ksipr2
                endif
                2003 format (' <CONTACT>        SE PROJETTE SUR LA MAILLE MAITRE ',&
     &        a8,' EN  <',e10.3,',',e10.3,'>')
!
! --------- REPERE LOCAL
!
                tau1(1) = zr(jtabf+ztabf*(iptc-1)+7 )
                tau1(2) = zr(jtabf+ztabf*(iptc-1)+8 )
                tau1(3) = zr(jtabf+ztabf*(iptc-1)+9 )
                tau2(1) = zr(jtabf+ztabf*(iptc-1)+10)
                tau2(2) = zr(jtabf+ztabf*(iptc-1)+11)
                tau2(3) = zr(jtabf+ztabf*(iptc-1)+12)
                write(ifm,2002) tau1(1),tau1(2),tau1(3), tau2(1),tau2(&
                2),tau2(3)
                2002 format (' <CONTACT>        TANGENTES : <',&
     &         e10.3,',',e10.3,',',e10.3,'> <',&
     &         e10.3,',',e10.3,',',e10.3,'>')
!
                call mmnorm(ndimg, tau1, tau2, norm, r8bid)
                write(ifm,2001) norm(1),norm(2),norm(3)
!
                2001 format (' <CONTACT>        NORMALE   : <',&
     &         e10.3,',',e10.3,',',e10.3,'>')
!
! --------- ETAT DE CONTACT
!
                xs = nint(zr(jtabf+ztabf*(iptc-1)+22))
!
                if (xs .eq. 0) then
                    write(ifm,7001)
                else if (xs.eq.1) then
                    write(ifm,7000)
                else
                    call assert(.false.)
                endif
                7000 format (' <CONTACT>        ETAT : EN CONTACT')
                7001 format (' <CONTACT>        ETAT : PAS EN CONTACT')
!
! --------- AUTRES INFOS
!
                seuili = zr(jtabf+ztabf*(iptc-1)+16)
                write(ifm,4002) seuili
                4002 format (' <CONTACT>        SEUIL_INIT : <',e10.3,'>')
!
! --------- LIAISON SUIVANTE
!
                iptc = iptc + 1
!
30          continue
20      continue
25      continue
10  end do
!
    call jedema()
!
end subroutine
