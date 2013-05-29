subroutine cfimp4(defico, resoco, noma, ifm)
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
! TOLE CRP_20
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/apinfi.h'
    include 'asterfort/apnomp.h'
    include 'asterfort/apvect.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfcald.h'
    include 'asterfort/cfconn.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfnben.h'
    include 'asterfort/cfnoap.h'
    include 'asterfort/cfnomm.h'
    include 'asterfort/cftypn.h'
    include 'asterfort/cfzonn.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mmnorm.h'
    character(len=24) :: defico
    character(len=24) :: resoco
    integer :: ifm
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! IMPRESSION DES INFOS DETAILLES DE L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ztacf
    character(len=24) :: tacfin
    integer :: jtacf
    character(len=24) :: apcoef, tangco
    integer :: japcoe, jtango
    character(len=24) :: apcofr
    integer :: japcof
    character(len=24) :: jeusup, jeuite
    integer :: jjeusu, jjeuit
    character(len=24) :: appoin, numlia
    integer :: japptr, jnumli
    character(len=24) :: nbddl, apddl
    integer :: jnbddl, japddl
    integer :: typapp, entapp
    integer :: nzoco, ntnoe, nbliai, nnoco, ndimg
    integer :: k, izone, ino, iliai, iddle, inom, iddlm, ip
    integer :: posno, posnoe, posmam
    character(len=8) :: nomno, nomnoe, nommam, nomnom, nomno2, nomapp
    integer :: posno2
    integer :: jdece, jdecm, jdecno
    integer :: nbddlt, nbddle, nbddlm, nbnom
    character(len=4) :: typno, type2
    character(len=19) :: sdappa
    character(len=16) :: nompt
    real(kind=8) :: jeuold, dissup
    real(kind=8) :: coefff, coefpn, coefpt, r8bid
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    logical :: lnodal, lctfd, lfrot
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(defico,'FROT_DISCRET')
    lfrot = cfdisl(defico,'FROTTEMENT')
!
    apcoef = resoco(1:14)//'.APCOEF'
    jeusup = resoco(1:14)//'.JSUPCO'
    tacfin = resoco(1:14)//'.TACFIN'
    tangco = resoco(1:14)//'.TANGCO'
    numlia = resoco(1:14)//'.NUMLIA'
    appoin = resoco(1:14)//'.APPOIN'
    nbddl = resoco(1:14)//'.NBDDL'
    apddl = resoco(1:14)//'.APDDL'
    jeuite = resoco(1:14)//'.JEUITE'
!
    call jeveuo(apcoef, 'L', japcoe)
    if (lctfd) then
        apcofr = resoco(1:14)//'.APCOFR'
        call jeveuo(apcofr, 'L', japcof)
    endif
    call jeveuo(jeusup, 'L', jjeusu)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(tangco, 'L', jtango)
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(nbddl, 'L', jnbddl)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(jeuite, 'L', jjeuit)
!
    ztacf = cfmmvd('ZTACF')
!
! --- SD APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM')
    nzoco = cfdisi(defico,'NZOCO')
    ntnoe = cfdisi(defico,'NTNOE')
    nbliai = cfdisd(resoco,'NBLIAI')
    nnoco = cfdisi(defico,'NNOCO')
!
    write(ifm,*) '<CONTACT><APPA> RESULTATS DE L''APPARIEMENT'
!
! ----------------------------------------------------------------------
! --- INFOS SUR LES ZONES DE CONTACT
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ------ ZONES ------ '
!
    write(ifm,1000) nzoco
    write(ifm,1001) ntnoe
    write(ifm,1002) nbliai
!
    1000 format (' <CONTACT><APPA> NOMBRE DE ZONES DE CONTACT        : ',&
     & i6)
    1001 format (' <CONTACT><APPA> NOMBRE MAXIMAL DE NOEUDS ESCLAVES : ',&
     &i6)
    1002 format (' <CONTACT><APPA> NOMBRE EFFECTIF DE LIAISONS       : ',&
     &i6)
!
!
! ----------------------------------------------------------------------
! --- INFOS SUR TOUS LES NOEUDS
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ------ NOEUDS DE CONTACT ------ '
!
    do 30 ino = 1, nnoco
!
! ----- TYPE DU NOEUD
!
        posno = ino
        call cftypn(defico, posno, typno)
!
! ----- NOM DU NOEUD
!
        call cfnomm(noma, defico, 'NOEU', posno, nomno)
!
        write(ifm,3000) ino,nomno,typno
!
! ----- ZONE
!
        call cfzonn(defico, posno, izone)
!
! ----- RECUPERATIONS DES TANGENTES AU NOEUD
!
        call apvect(sdappa, 'APPARI_NOEUD_TAU1', posno, tau1)
        call apvect(sdappa, 'APPARI_NOEUD_TAU2', posno, tau2)
!
! ----- NORMALE
!
        if (typno .eq. 'MAIT') then
            if (cfcald(defico,izone ,'MAIT')) then
                write(ifm,3003) (tau1(k),k=1,3)
                if (ndimg .eq. 3) then
                    write(ifm,3005) (tau2(k),k=1,3)
                endif
                call mmnorm(ndimg, tau1, tau2, norm, r8bid)
                write(ifm,3006) (norm(k),k=1,3)
            else
                write(ifm,3007)
            endif
        else
            if (cfcald(defico,izone ,'ESCL')) then
                write(ifm,3003) (tau1(k),k=1,3)
                if (ndimg .eq. 3) then
                    write(ifm,3005) (tau2(k),k=1,3)
                endif
                call mmnorm(ndimg, tau1, tau2, norm, r8bid)
                write(ifm,3006) (norm(k),k=1,3)
            else
                write(ifm,3007)
            endif
        endif
!
!
        3000 format (' <CONTACT><APPA> NOEUD NUMERO ',i6,' (',a8,') -> NOEUD ',&
     &           a4)
        3003 format (' <CONTACT><APPA>  * TANGENTE 1  : ',3(1pe15.8,2x))
        3005 format (' <CONTACT><APPA>  * TANGENTE 2  : ',3(1pe15.8,2x))
        3006 format (' <CONTACT><APPA>  * NORMALE     : ',3(1pe15.8,2x))
        3007 format (' <CONTACT><APPA>  * TANGENTE ET NORMALE NON CALCULEES')
!
!
30  end do
!
! ----------------------------------------------------------------------
! --- INFOS SUR LES NOEUDS ESCLAVES
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ----- NOEUDS ESCLAVES ----- '
!
    do 40 iliai = 1, nbliai
!
! ----- POINT DE CONTACT
!
        ip = zi(jnumli+4*(iliai-1)+1-1)
!
! ----- NOEUD ESCLAVE
!
        posnoe = zi(jnumli+4*(iliai-1)+2-1)
!
! ----- INFOS APPARIEMENT
!
        call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
        call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
        call apinfi(sdappa, 'APPARI_ZONE', ip, izone)
!
! ----- NOM DU NOEUD ESCLAVE
!
        call apnomp(sdappa, ip, nompt)
        nomnoe = nompt(9:16)
        write(ifm,4000) iliai,nompt
!
! ----- NOM ET TYPE DU MAITRE
!
        call cfnoap(noma, defico, typapp, entapp, nomapp,&
                    type2)
        if (typapp .lt. 0) then
            write(ifm,4003)
            lnodal = .false.
        else if (typapp.eq.1) then
            write(ifm,4001) nomapp
            nomnom = nomapp
            lnodal = .true.
        else if (typapp.eq.2) then
            write(ifm,4002) nomapp
            nommam = nomapp
            lnodal = .false.
        else
            call assert(.false.)
        endif
!
! --- NOMBRE DE DDLS TOTAL: NBDDLT
!
        nbddlt = zi(japptr+iliai) - zi(japptr+iliai-1)
        nbddle = zi(jnbddl+posnoe) - zi(jnbddl+posnoe-1)
!
! --- AFFICHAGES
!
        write(ifm,4004) nbddlt,nbddle
!
! ----- TANGENTES ET NORMALE
!
        tau1(1) = zr(jtango+6*(iliai-1)+1-1)
        tau1(2) = zr(jtango+6*(iliai-1)+2-1)
        tau1(3) = zr(jtango+6*(iliai-1)+3-1)
        tau2(1) = zr(jtango+6*(iliai-1)+4-1)
        tau2(2) = zr(jtango+6*(iliai-1)+5-1)
        tau2(3) = zr(jtango+6*(iliai-1)+6-1)
        call mmnorm(ndimg, tau1, tau2, norm, r8bid)
        write(ifm,4007) (norm(k),k=1,3)
        write(ifm,4008) (tau1(k),k=1,3)
        if (ndimg .eq. 3) then
            write(ifm,5008) (tau2(k),k=1,3)
        endif
!
! ----- JEUX
!
        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
        dissup = zr(jjeusu+iliai-1)
        write(ifm,4006) jeuold,dissup
!
! ----- PARAMETRES PENALISATION ET FROTTEMENT
!
        coefff = zr(jtacf+ztacf*(iliai-1)+0)
        coefpn = zr(jtacf+ztacf*(iliai-1)+1)
        coefpt = zr(jtacf+ztacf*(iliai-1)+2)
        write(ifm,7000) coefpn
        write(ifm,7001) coefpt
        write(ifm,7003) coefff
!
! ----- DDL ET COEF CONTACT POUR NOEUD ESCLAVE
!
        jdece = zi(japptr+iliai-1)
        if (ndimg .eq. 3) then
            write (ifm,4015) nomnoe, (zi(japddl+jdece+iddle-1),iddle=&
            1,nbddle), (zr(japcoe+jdece+iddle-1),iddle=1,nbddle)
        else
            write (ifm,4018) nomnoe, (zi(japddl+jdece+iddle-1),iddle=&
            1,nbddle), (zr(japcoe+jdece+iddle-1),iddle=1,nbddle)
        endif
!
! ----- DDL ET COEF FROTTEMENT POUR NOEUD ESCLAVE
!
        jdece = zi(japptr+iliai-1)
        if (lfrot) then
            if (ndimg .eq. 3) then
                write (ifm,4016) nomnoe, (zi(japddl+jdece+iddle-1),&
                iddle=1,nbddle), (zr(japcof+jdece+iddle-1),iddle=1,&
                nbddle)
                write (ifm,4017) nomnoe, (zi(japddl+jdece+iddle-1),&
                iddle=1,nbddle), (zr(japcof+30*ntnoe+jdece+iddle-1),&
                iddle=1,nbddle)
            else
                write (ifm,4019) nomnoe, (zi(japddl+jdece+iddle-1),&
                iddle=1,nbddle), (zr(japcof+jdece+iddle-1),iddle=1,&
                nbddle)
            endif
        endif
!
! ----------------------------------------------------------------------
! --- DDL ET COEF POUR NOEUDS MAITRES
! ----------------------------------------------------------------------
!
        jdecm = jdece + nbddle
!
! ----- APPARIEMENT NODAL
!
        if (lnodal) then
            nbnom = 1
            inom = 1
            nbddlm = nbddlt - nbddle
!
! ------- COEFFICIENTS POUR CONTACT
!
            if (ndimg .eq. 3) then
                write (ifm,5011) nomnom, (zi(japddl+jdecm+(inom-1)*&
                nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(japcoe+jdecm+(&
                inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
            else
                write (ifm,6011) nomnom, (zi(japddl+jdecm+(inom-1)*&
                nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(japcoe+jdecm+(&
                inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
            endif
!
! ------- COEFFICIENTS POUR FROTTEMENT
!
            if (lfrot) then
                if (ndimg .eq. 3) then
                    write (ifm,5012) nomnom, (zi(japddl+jdecm+(inom-1)&
                    *nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(japcoe+&
                    jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                    write (ifm,5013) nomnom, (zi(japddl+jdecm+(inom-1)&
                    *nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(japcof+30*&
                    ntnoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                    nbddlm)
                else
                    write (ifm,6012) nomnom, (zi(japddl+jdecm+(inom-1)&
                    *nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(japcoe+&
                    jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                endif
            endif
!
! ----- APPARIEMENT MAITRE/ESCLAVE
!
        else
            posmam = entapp
            call cfnben(defico, posmam, 'CONNEX', nbnom, jdecno)
            do 50 inom = 1, nbnom
                call cfconn(defico, jdecno, inom, posno2)
                nbddlm = zi(jnbddl+posno2) - zi(jnbddl+posno2-1)
!
                call cfnomm(noma, defico, 'NOEU', posno2, nomno2)
!
! --- COEFFICIENTS POUR CONTACT
!
                if (ndimg .eq. 3) then
                    write (ifm,5001) nommam,nomno2, (zi(japddl+jdecm+(&
                    inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(&
                    japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                    nbddlm)
                else
                    write (ifm,6001) nommam,nomno2, (zi(japddl+jdecm+(&
                    inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm), (zr(&
                    japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                    nbddlm)
                endif
!
! --- COEFFICIENTS POUR FROTTEMENT
!
                if (lfrot) then
                    if (ndimg .eq. 3) then
                        write (ifm,5002) nommam,nomno2, (zi(japddl+&
                        jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                        nbddlm), (zr(japcoe+jdecm+(inom-1)*nbddlm+&
                        iddlm-1), iddlm=1,nbddlm)
                        write (ifm,5003) nommam,nomno2, (zi(japddl+&
                        jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                        nbddlm), (zr(japcof+30*ntnoe+jdecm+(inom-1)*&
                        nbddlm+iddlm-1), iddlm=1,nbddlm)
                    else
                        write (ifm,6002) nommam,nomno2, (zi(japddl+&
                        jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,&
                        nbddlm), (zr(japcoe+jdecm+(inom-1)*nbddlm+&
                        iddlm-1), iddlm=1,nbddlm)
                    endif
                endif
50          continue
!
        endif
!
40  end do
!
    4000 format (' <CONTACT><APPA> LIAISON NUMERO ',i6,' (',&
     &        a16,')')
!
    4001 format (' <CONTACT><APPA>  * APPARIEMENT AVEC NOEUD  ',a8)
    4002 format (' <CONTACT><APPA>  * APPARIEMENT AVEC MAILLE ',a8)
    4003 format (' <CONTACT><APPA>  * NON APPARIE')
    4004 format (' <CONTACT><APPA>  * NOMBRE DE DDLS : ',i6,' DONT ',i6,&
     &                        ' POUR NOEUD ESCLAVE',i6)
!
    4006 format (' <CONTACT><APPA>  * JEU: ',1pe15.8,' DONT :',1pe15.8,&
     &        ' VENANT DE DIST_*')
    4007 format (' <CONTACT><APPA>  * NORMALE LISSEE/MOYENNEE: ',&
     &         3(1pe15.8,2x))
    4008 format (' <CONTACT><APPA>  * TANGENTE DIRECTION 1   : ',&
     &         3(1pe15.8,2x))
    5008 format (' <CONTACT><APPA>  * TANGENTE DIRECTION 2   : ',&
     &         3(1pe15.8,2x))
!
!
! --- 3D - ESCLAVE
!
    4015 format ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
!
    4016 format ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
    4017 format ((' <CONTACT><APPA>  * DDL ESCL. FROT2   ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
!
!
! --- 2D - ESCLAVE
!
    4018 format ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
!
    4019 format ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
!
!
! --- 3D MAITRE/ESCL - MAITRE
!
    5001 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,&
     &           '/',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
    5002 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,&
     &           '/',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
    5003 format ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',a8,&
     &           '/',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
!
! --- 3D NODAL - MAITRE
!
    5011 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
    5012 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
    5013 format ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',a8,'):',&
     &           3(i8,2x),' / ',&
     &           3(1pe15.8,2x)))
!
! --- 2D MAITRE/ESCL - MAITRE
!
    6001 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,&
     &           '/',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
    6002 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,&
     &           '/',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
!
! --- 2D NODAL - MAITRE
!
    6011 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
    6012 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,'):',&
     &           2(i8,2x),' / ',&
     &           2(1pe15.8,2x)))
!
    7000 format (' <CONTACT><APPA>  * E_N              :',1pe15.8)
    7001 format (' <CONTACT><APPA>  * E_T              :',1pe15.8)
    7003 format (' <CONTACT><APPA>  * COULOMB          :',1pe15.8)
!
!
!
    call jedema()
!
end subroutine
