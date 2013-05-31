subroutine mmmres(noma, inst, defico, resoco, depplu,&
                  depdel, sddisc, veasse, cnsinr, cnsper)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/iseven.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/mcopco.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/mminfm.h'
    include 'asterfort/mmmred.h'
    include 'asterfort/mmmreg.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: noma
    real(kind=8) :: inst(*)
    character(len=19) :: cnsinr, cnsper, sddisc
    character(len=19) :: veasse(*)
    character(len=24) :: defico, resoco
    character(len=19) :: depdel, depplu
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
!
! REMPLIR LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
! IN  DEPPLU : DEPLACEMENT COURANT
! IN  NOMA   : NOM DU MAILLAGE
! IN  INST   : PARAMETRES INTEGRATION EN TEMPS (T+, DT, THETA)
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
! OUT CNSPER : CHAM_NO_S POUR L'ARCHIVAGE DES PERCUSSIONS
!
! ----------------------------------------------------------------------
!
    integer :: iptc
    integer :: izone, imae, iptm
    integer :: ndd1, nptm, nbmae
    integer :: jcont, jfrot
    integer :: iacnx1, ilcnx1
    integer :: nzoco
    integer :: jdepde, jdeplu
    integer :: ndimg
    integer :: ztabf, zresu, zperc
    integer :: numnoe, nummam
    integer :: posmae
    integer :: jdecme
    real(kind=8) :: gli, gli1, gli2
    real(kind=8) :: rn, rnx, rny, rnz
    real(kind=8) :: rtax, rtay, rtaz
    real(kind=8) :: rtgx, rtgy, rtgz
    real(kind=8) :: r, rx, ry, rz
    real(kind=8) :: imp, impx, impy, impz
    real(kind=8) :: cont, lagsf
    real(kind=8) :: ksipr1, ksipr2, proj(3)
    character(len=8) :: licnt3(3)
    character(len=19) :: fconts, ffrots, depdes, depcn
    character(len=19) :: fctcn, ffrocn
    character(len=19) :: fcont, ffrot
    character(len=19) :: newgeo
    character(len=24) :: glie, glim
    integer :: jglie, jglim
    character(len=24) :: tabfin, apjeu
    integer :: jtabf, japjeu
    real(kind=8) :: deltat, eps, valras
    integer :: ifm, niv
    logical :: lfrot, lveri, lnoeu
    logical :: lcolli, laffle
    integer :: jcnsvr, jcnslr, jcnsvp, jcnslp
    parameter (eps=1.d-6)
! ----------------------------------------------------------------------
    data licnt3&
     &   / 'DX'     ,'DY'      ,'DZ'      /
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- TYPE DE CONTACT
!
    lfrot = cfdisl(defico,'FROTTEMENT')
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO' )
    ndimg = cfdisi(defico,'NDIM' )
    deltat = inst(2)
!
! --- TOUTES LES ZONES EN INTEGRATION AUX NOEUDS ?
!
    lnoeu = cfdisl(defico,'ALL_INTEG_NOEUD')
    if (.not.lnoeu) then
        goto 999
    endif
!
! --- CONTACT AFFLEURANT EN MODE COLLISION
!
    laffle = .false.
    valras = 1.d-3
    call iseven(sddisc, 'COLLISION', lcolli)
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    apjeu = resoco(1:14)//'.APJEU '
    call jeveuo(tabfin, 'L', jtabf)
    call jeveuo(apjeu, 'L', japjeu)
!
    ztabf = cfmmvd('ZTABF')
    zperc = cfmmvd('ZPERC')
    zresu = cfmmvd('ZRESU')
!
! --- NOM DES OBJETS LOCAUX
!
    fcont = '&&MMMRES.CONT'
    fconts = '&&MMMRES.CONT_S'
    fctcn = '&&MMMRES.FCTCN'
    ffrot = '&&MMMRES.FROT'
    ffrots = '&&MMMRES.FROT_S'
    ffrocn = '&&MMMRES.FROTCN'
    depdes = '&&MMMRES.DEPDES'
    depcn = '&&MMMRES.DEPCN'
    glie = '&&MMMRES.GLIE'
    glim = '&&MMMRES.GLIM'
!
! --- GEOMETRIE ACTUALISEE
!
    newgeo = resoco(1:14)//'.NEWG'
!
! --- ACCES AU MAILLAGE
!
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', iacnx1)
!
! --- REDUCTION DU CHAM_NO_S DES DDL EN UN CHAM_NO_S DES LAGRANGES
! --- DE CONTACT/FROTTEMENT
!
    call mmmred(ndimg, lfrot, depdel, depcn, ndd1)
!
! --- CALCULER LES GLISSEMENTS
!
    call mmmreg(noma, defico, resoco, depcn, ndd1,&
                glie, glim)
    call jeveuo(glie, 'L', jglie)
    call jeveuo(glim, 'L', jglim)
!
! --- ACCES AU CHAM_NO_S POUR LES DEPLACEMENTS/LAGRANGES
!
    call jeveuo(depcn(1:19)//'.CNSV', 'L', jdepde)
    call jeveuo(depplu(1:19)//'.VALE', 'L', jdeplu)
!
! --- ACCES AU CHAM_NO_S POUR LE CONTACT
!
    call jeveuo(cnsinr(1:19)//'.CNSV', 'E', jcnsvr)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'E', jcnslr)
!
! --- ACCES AU CHAM_NO_S POUR LES PERCUSSIONS
! --- ON NE REMET PAS A ZERO D'UN PAS A L'AUTRE
!
    call jeveuo(cnsper(1:19)//'.CNSV', 'E', jcnsvp)
    call jeveuo(cnsper(1:19)//'.CNSL', 'E', jcnslp)
!
! --- FORCES NODALES DE CONTACT
!
    call nmchex(veasse, 'VEASSE', 'CNELTC', fcont)
    call cnocns(fcont, 'V', fconts)
    call cnsred(fconts, 0, 0, ndimg, licnt3,&
                'V', fctcn)
    call jeveuo(fctcn//'.CNSV', 'L', jcont)
!
! --- FORCES NODALES DE FROTTEMENT
!
    if (lfrot) then
        call nmchex(veasse, 'VEASSE', 'CNELTF', ffrot)
        call cnocns(ffrot, 'V', ffrots)
        call cnsred(ffrots, 0, 0, ndimg, licnt3,&
                    'V', ffrocn)
        call jeveuo(ffrocn//'.CNSV', 'L', jfrot)
    endif
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do 10 izone = 1, nzoco
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
        lfrot = mminfl(defico,'FROTTEMENT_ZONE',izone)
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        if (lveri) then
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- POSITION DE LA MAILLE ESCLAVE
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
! --------- INIT
!
                gli = 0.d0
                gli1 = 0.d0
                gli2 = 0.d0
                rtax = 0.d0
                rtay = 0.d0
                rtaz = 0.d0
                rtgx = 0.d0
                rtgy = 0.d0
                rtgz = 0.d0
                rn = 0.d0
                rnx = 0.d0
                rny = 0.d0
                rnz = 0.d0
!
! --------- INFOS
!
                numnoe = nint(zr(jtabf+ztabf*(iptc-1)+24))
                if (numnoe .le. 0) then
                    goto 99
                endif
                cont = zr(jtabf+ztabf*(iptc-1)+22)
!
! --------- RECUPERATION DES FORCES NODALES DE CONTACT
!
                if (cont .ge. 1.d0) then
                    if (ndimg .eq. 3) then
                        rnx = zr(jcont-1+3*(numnoe-1)+1)
                        rny = zr(jcont-1+3*(numnoe-1)+2)
                        rnz = zr(jcont-1+3*(numnoe-1)+3)
                        rn = sqrt(rnx**2+rny**2+rnz**2)
                    else if (ndimg.eq.2) then
                        rnx = zr(jcont-1+2*(numnoe-1)+1)
                        rny = zr(jcont-1+2*(numnoe-1)+2)
                        rn = sqrt(rnx**2+rny**2)
                    else
                        call assert(.false.)
                    endif
!
! ----------- CONTACT AFFLEURANT ?
!
                    if (rn .le. valras) then
                        laffle = .true.
                    endif
!
! --------- FROTTEMENT
!
                    if (lfrot) then
!
! ------------- CALCUL DU GLISSEMENT
!
                        if (ndimg .eq. 3) then
                            gli1 = zr(jglie+2*(iptc-1))-zr(jglim+2*( iptc-1))
                            gli2 = zr( jglie+2*(iptc-1)+1) - zr(jglim+ 2*(iptc-1)+1 )
                            gli = sqrt(gli1**2+gli2**2)
                        else if (ndimg.eq.2) then
                            gli1 = zr(jglie+iptc-1) - zr(jglim+iptc-1)
                            gli = abs(gli1)
                        else
                            call assert(.false.)
                        endif
!
! ------------- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
!
                        if (ndimg .eq. 3) then
                            lagsf = sqrt(&
                                    zr(&
                                    jdepde-1+ndd1*(numnoe-1)+ 5)**2+ zr(jdepde-1+ndd1*(numnoe-1)+&
                                    &6&
                                    )**2&
                                    )
                        else if (ndimg.eq.2) then
                            lagsf = abs (zr(jdepde-1+ndd1*(numnoe-1)+ 4))
                        else
                            call assert(.false.)
                        endif
!
! ------------- GLISSANT OU ADHERENT
!
                        if (lagsf .ge. 0.999d0) then
!
! --------------- GLISSANT
!
                            if (ndimg .eq. 3) then
                                cont = 2.d0
                                rtgx = zr(jfrot-1+3*(numnoe-1)+1)
                                rtgy = zr(jfrot-1+3*(numnoe-1)+2)
                                rtgz = zr(jfrot-1+3*(numnoe-1)+3)
                            else if (ndimg.eq.2) then
                                cont = 2.d0
                                rtgx = zr(jfrot-1+2*(numnoe-1)+1)
                                rtgy = zr(jfrot-1+2*(numnoe-1)+2)
                            else
                                call assert(.false.)
                            endif
                        else
!
! --------------- ADHERENT
!
                            if (ndimg .eq. 3) then
                                rtax = zr(jfrot-1+3*(numnoe-1)+1)
                                rtay = zr(jfrot-1+3*(numnoe-1)+2)
                                rtaz = zr(jfrot-1+3*(numnoe-1)+3)
                            else if (ndimg.eq.2) then
                                rtax = zr(jfrot-1+2*(numnoe-1)+1)
                                rtay = zr(jfrot-1+2*(numnoe-1)+2)
                            else
                                call assert(.false.)
                            endif
                        endif
                    else
                        lagsf = 0.d0
                        cont = 2.d0
                    endif
                endif
!
! --------- REACTIONS TOTALES
!
                rx = rnx + rtax + rtgx
                ry = rny + rtay + rtgy
                rz = rnz + rtaz + rtgz
                r = sqrt(rx**2.d0+ry**2.d0+rz**2.d0)
!
! --------- CALCUL DES PERCUSSIONS
!
                if (r .le. eps) then
                    imp = 0.d0
                    impx = 0.d0
                    impy = 0.d0
                    impz = 0.d0
                else
                    imp = zr(jcnsvp+zperc*(numnoe-1)+1-1) + r*deltat
                    impx = zr(jcnsvp+zperc*(numnoe-1)+2-1) + rx* deltat
                    impy = zr(jcnsvp+zperc*(numnoe-1)+3-1) + ry* deltat
                    impz = zr(jcnsvp+zperc*(numnoe-1)+4-1) + rz* deltat
                endif
!
! --------- COORDONNNES DE LA PROJECTION
!
                ksipr1 = zr(jtabf+ztabf*(iptc-1)+5 )
                ksipr2 = zr(jtabf+ztabf*(iptc-1)+6 )
                nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
                call mcopco(noma, newgeo, ndimg, nummam, ksipr1,&
                            ksipr2, proj)
!
! --------- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S VALE_CONT
!
                zr(jcnsvr+zresu*(numnoe-1)+1 -1) = cont
                zr(jcnsvr+zresu*(numnoe-1)+2 -1) = -zr(japjeu+iptc-1)
                zl(jcnslr+zresu*(numnoe-1)+1 -1) = .true.
                zl(jcnslr+zresu*(numnoe-1)+2 -1) = .true.
                if (ndimg .eq. 3) then
                    zr(jcnsvr+zresu*(numnoe-1)+3 -1) = rn
                    zr(jcnsvr+zresu*(numnoe-1)+4 -1) = rnx
                    zr(jcnsvr+zresu*(numnoe-1)+5 -1) = rny
                    zr(jcnsvr+zresu*(numnoe-1)+6 -1) = rnz
                    zr(jcnsvr+zresu*(numnoe-1)+7 -1) = gli1
                    zr(jcnsvr+zresu*(numnoe-1)+8 -1) = gli2
                    zr(jcnsvr+zresu*(numnoe-1)+9 -1) = gli
                    zr(jcnsvr+zresu*(numnoe-1)+10-1) = rtax
                    zr(jcnsvr+zresu*(numnoe-1)+11-1) = rtay
                    zr(jcnsvr+zresu*(numnoe-1)+12-1) = rtaz
                    zr(jcnsvr+zresu*(numnoe-1)+13-1) = rtgx
                    zr(jcnsvr+zresu*(numnoe-1)+14-1) = rtgy
                    zr(jcnsvr+zresu*(numnoe-1)+15-1) = rtgz
                    zr(jcnsvr+zresu*(numnoe-1)+16-1) = rx
                    zr(jcnsvr+zresu*(numnoe-1)+17-1) = ry
                    zr(jcnsvr+zresu*(numnoe-1)+18-1) = rz
                    zr(jcnsvr+zresu*(numnoe-1)+19-1) = r
                    zr(jcnsvr+zresu*(numnoe-1)+21-1) = imp
                    zr(jcnsvr+zresu*(numnoe-1)+22-1) = impx
                    zr(jcnsvr+zresu*(numnoe-1)+23-1) = impy
                    zr(jcnsvr+zresu*(numnoe-1)+24-1) = impz
                    zr(jcnsvr+zresu*(numnoe-1)+28-1) = proj(1)
                    zr(jcnsvr+zresu*(numnoe-1)+29-1) = proj(2)
                    zr(jcnsvr+zresu*(numnoe-1)+30-1) = proj(3)
                    zl(jcnslr+zresu*(numnoe-1)+3 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+4 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+5 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+6 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+7 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+8 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+9 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+10-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+11-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+12-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+13-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+14-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+15-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+16-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+17-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+18-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+19-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+21-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+22-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+23-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+24-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+28-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+29-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+30-1) = .true.
                else if (ndimg.eq.2) then
                    zr(jcnsvr+zresu*(numnoe-1)+3 -1) = rn
                    zr(jcnsvr+zresu*(numnoe-1)+4 -1) = rnx
                    zr(jcnsvr+zresu*(numnoe-1)+5 -1) = rny
                    zr(jcnsvr+zresu*(numnoe-1)+7 -1) = gli1
                    zr(jcnsvr+zresu*(numnoe-1)+9 -1) = gli
                    zr(jcnsvr+zresu*(numnoe-1)+10-1) = rtax
                    zr(jcnsvr+zresu*(numnoe-1)+11-1) = rtay
                    zr(jcnsvr+zresu*(numnoe-1)+13-1) = rtgx
                    zr(jcnsvr+zresu*(numnoe-1)+14-1) = rtgy
                    zr(jcnsvr+zresu*(numnoe-1)+16-1) = rx
                    zr(jcnsvr+zresu*(numnoe-1)+17-1) = ry
                    zr(jcnsvr+zresu*(numnoe-1)+19-1) = r
                    zr(jcnsvr+zresu*(numnoe-1)+21-1) = imp
                    zr(jcnsvr+zresu*(numnoe-1)+22-1) = impx
                    zr(jcnsvr+zresu*(numnoe-1)+23-1) = impy
                    zr(jcnsvr+zresu*(numnoe-1)+28-1) = proj(1)
                    zr(jcnsvr+zresu*(numnoe-1)+29-1) = proj(2)
                    zl(jcnslr+zresu*(numnoe-1)+3 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+4 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+5 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+7 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+9 -1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+10-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+11-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+13-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+14-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+16-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+17-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+19-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+21-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+22-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+23-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+28-1) = .true.
                    zl(jcnslr+zresu*(numnoe-1)+29-1) = .true.
                else
                    call assert(.false.)
                endif
!
! --------- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S VALE_PERC
!
                zr(jcnsvp+zperc*(numnoe-1)+1-1) = imp
                zr(jcnsvp+zperc*(numnoe-1)+2-1) = impx
                zr(jcnsvp+zperc*(numnoe-1)+3-1) = impy
                zl(jcnslp+zperc*(numnoe-1)+1-1) = .true.
                zl(jcnslp+zperc*(numnoe-1)+2-1) = .true.
                zl(jcnslp+zperc*(numnoe-1)+3-1) = .true.
!
                if (ndimg .eq. 3) then
                    zr(jcnsvp+zperc*(numnoe-1)+4-1) = impz
                    zl(jcnslp+zperc*(numnoe-1)+4-1) = .true.
                endif
99              continue
!
! --------- LIAISON DE CONTACT SUIVANTE
!
                iptc = iptc + 1
30          continue
20      continue
25      continue
10  end do
!
! --- MENAGE
!
    call jedetr(fcont)
    call detrsd('CHAMP', fconts)
    call detrsd('CHAMP', fctcn)
    call jedetr(ffrot)
    call detrsd('CHAMP', ffrots)
    call detrsd('CHAMP', ffrocn)
    call detrsd('CHAMP', depdes)
    call detrsd('CHAMP', depcn)
    call jedetr(glie)
    call jedetr(glim)
!
! --- ALARME SI CONTACT AFFLEURANT EN MODE DETECTION COLLISION
!
    if (laffle .and. lcolli) then
        call u2mess('A', 'CONTACT3_98')
    endif
!
999  continue
    call jedema()
end subroutine
