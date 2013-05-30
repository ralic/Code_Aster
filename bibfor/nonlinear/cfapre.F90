subroutine cfapre(noma, defico, resoco, newgeo, sdappa)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/apinfi.h'
    include 'asterfort/apinfr.h'
    include 'asterfort/apvect.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfapma.h'
    include 'asterfort/cfapno.h'
    include 'asterfort/cfappi.h'
    include 'asterfort/cfcorn.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfdist.h'
    include 'asterfort/cfecrd.h'
    include 'asterfort/cfmmco.h'
    include 'asterfort/cfnumn.h'
    include 'asterfort/cfparz.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfr.h'
    character(len=8) :: noma
    character(len=19) :: sdappa
    character(len=24) :: defico, resoco
    character(len=19) :: newgeo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT DEDIEE POUR LE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NEWGEO : NOUVELLE GEOMETRIE (AVEC DEPLACEMENT GEOMETRIQUE)
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: izone, i, iliai, ip
    integer :: jdecne
    integer :: inoe
    integer :: posmae, posnoe, posmam, posnom
    integer :: numnoe
    integer :: entapp, typapp
    logical :: lctfd, lctf3d
    integer :: nzoco, ndimg, nbpt, nbliai
    integer :: nesmax
    logical :: lveri
    character(len=8) :: nomnoe
    real(kind=8) :: ksipr1, ksipr2, tau1m(3), tau2m(3)
    real(kind=8) :: coorne(3), dissup
    real(kind=8) :: coefff, coefpn, coefpt, coefte
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... RECOPIE DE L''APPARIEMENT'
    endif
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(defico,'FROT_DISCRET')
    lctf3d = cfdisl(defico,'FROT_3D' )
!
! --- NOMBRE TOTAL DE NOEUDS ESCLAVES ET DIMENSION DU PROBLEME
!
    nzoco = cfdisi(defico,'NZOCO' )
    ndimg = cfdisi(defico,'NDIM' )
!
! --- INITIALISATIONS
!
    ip = 1
    iliai = 0
    posmae = 0
!
! --- BOUCLE SUR LES ZONES
!
    do 10 izone = 1, nzoco
!
! ----- INFORMATION SUR LA ZONE
!
        nbpt = mminfi(defico,'NBPT' ,izone )
        jdecne = mminfi(defico,'JDECNE',izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            ip = ip + nbpt
            goto 25
        endif
!
! ----- COEFFICIENTS
!
        call cfmmco(defico, resoco, izone, 'E_N', 'L',&
                    coefpn)
        call cfmmco(defico, resoco, izone, 'E_T', 'L',&
                    coefpt)
        coefff = mminfr(defico,'COEF_COULOMB' ,izone )
        coefte = mminfr(defico,'COEF_MATR_FROT' ,izone )
!
! ----- BOUCLE SUR LES NOEUDS DE CONTACT
!
        do 20 i = 1, nbpt
!
! ------- NOEUD ESCLAVE COURANT
!
            inoe = i
            posnoe = jdecne + inoe
!
! ------- INDICE ABSOLU DANS LE MAILLAGE DU NOEUD
!
            call cfnumn(defico, 1, posnoe, numnoe)
!
! ------- COORDONNEES DU NOEUD
!
            call cfcorn(newgeo, numnoe, coorne)
!
! ------- NOM DU NOEUD
!
            call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
!
! ------- INFOS APPARIEMENT
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
            call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksipr1)
            call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksipr2)
            call apvect(sdappa, 'APPARI_TAU1', ip, tau1m)
            call apvect(sdappa, 'APPARI_TAU2', ip, tau2m)
!
! ------- RECOPIE APPARIEMENT
!
            if (typapp .lt. 0) then
                if (niv .ge. 2) then
                    call cfappi(noma, defico, nomnoe, typapp, entapp)
                endif
                goto 35
            else if (typapp.eq.1) then
! --------- CARAC. MAITRE
                posnom = entapp
! --------- LIAISON DE CONTACT EFFECTIVE
                iliai = iliai + 1
! --------- CALCUL LIAISON
                call cfapno(noma, newgeo, defico, resoco, lctfd,&
                            lctf3d, ndimg, izone, posnoe, numnoe,&
                            coorne, posnom, tau1m, tau2m, iliai)
!
            else if (typapp.eq.2) then
! --------- CARAC. MAITRE
                posmam = entapp
! --------- LIAISON DE CONTACT EFFECTIVE
                iliai = iliai + 1
! --------- CALCUL LIAISON
                call cfapma(noma, newgeo, defico, resoco, lctfd,&
                            lctf3d, ndimg, izone, posnoe, numnoe,&
                            coorne, posmam, ksipr1, ksipr2, tau1m,&
                            tau2m, iliai)
            else
                call assert(.false.)
            endif
!
! ------- CALCUL DU JEU FICTIF DE LA ZONE
!
            call cfdist(defico, 'DISCRETE', izone, posnoe, posmae,&
                        coorne, dissup)
!
! ------- CARACTERISTIQUES DE LA LIAISON POUR LA ZONE
!
            call cfparz(resoco, iliai, coefff, coefpn, coefpt,&
                        coefte, dissup, izone, ip, numnoe,&
                        posnoe)
!
35          continue
!
! ------- POINT SUIVANT
!
            ip = ip + 1
            call assert(iliai.le.ip)
!
20      continue
25      continue
10  end do
!
! --- NOMBRE DE LIAISONS EFFECTIVES
!
    nbliai = iliai
    call cfecrd(resoco, 'NBLIAI', nbliai)
    nesmax = cfdisd(resoco,'NESMAX')
    call assert(nbliai.le.nesmax)
!
    call jedema()
end subroutine
