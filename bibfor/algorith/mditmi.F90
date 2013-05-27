subroutine mditmi(typflu, nombm, icoupl, nbm0, nbmode,&
                  nbmd, vgap, itrans, eps, ts,&
                  nts, itypfl)
!
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE
! -----------   STRUCTURE PAR UNE METHODE INTEGRALE
!               RECUPERATION DES DONNEES
!
!               APPELANT : MDTR74
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! -------------------------
!
! ARGUMENTS
! ---------
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8depi.h'
    include 'asterfort/copmod.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mdconf.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: typflu, nombm
    integer :: icoupl, nbm0, nbmode, nbmd, jpuls, jmasg, jamo1, jlocf, itypfl
    integer :: numvif, jbase, jamog
    real(kind=8) :: vgap
    integer :: iveci1, ivecr1, ivecr2, ivecr3, ivecr4, ivecr5, itrans
    real(kind=8) :: eps, ts
    integer :: nts, nbmp
    integer :: vali(3)
!
! VARIABLES LOCALES
! -----------------
    integer :: iam, ib, idiff, ie, im, indic, iv, j, jabsc, jbasf, jcodim, jnuor
    integer :: jphie, jpoids, jrhoe, kchref, kdrif, kfreq, kfsic, kmasg, knumo
    integer :: krefe, kvite, lamog, lires, lmasg, lomeg, lprofv, n1, n2, nbamor
    integer :: nbmcfc, nbnoeu, neq, nmp, nbtrou
    real(kind=8) :: deuxpi, r8b, xamor
    character(len=3) :: ouinon, k3iv, k3im
    character(len=4) :: k4b
    character(len=8) :: k8b, listam, mailla, matass
    character(len=14) :: numddl
    character(len=19) :: basefl
    character(len=24) :: fsic, chrefe
    character(len=24) :: valk(3)
    complex(kind=8) :: c16b
!
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC     SQRT
!
! FONCTIONS EXTERNES
! ------------------
    integer :: iarg
!     EXTERNAL      R8DEPI
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL      COPMOD, DISMOI, GETVID, GETVIS, GETVR8, GETVTX,
!    &              JEDEMA, JELIRA, JEMARQ, JEVEUO, MDCONF, RSADPA,
!    &              RSORAC,
!    &              WKVECT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
    deuxpi = r8depi()
    iv = 1
!
!
! 1.  RECUPERATION DU CONCEPT MELASFLU
!     --------------------------------
    call getvid('SCHEMA_TEMPS', 'BASE_ELAS_FLUI', 1, iarg, 1,&
                basefl, n1)
    call jeveuo(basefl//'.REMF', 'L', krefe)
    typflu = zk8(krefe)
    nombm = zk8(krefe+1)
!
!
! 2.  CARACTERISATION DU TYPE DE LA CONFIGURATION ETUDIEE
!     ---------------------------------------------------
    fsic = typflu//'           .FSIC'
    call jeveuo(fsic, 'L', kfsic)
    itypfl = zi(kfsic)
    icoupl = zi(kfsic+1)
!
    if ((itypfl.ne.1) .and. (itypfl.ne.2)) call u2mess('F', 'ALGORITH5_54')
!
    if (icoupl .ne. 1) call u2mess('A', 'ALGORITH5_55')
!
!
! 3.  RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
!     --------------------------------------------
    call jelira(basefl//'.NUMO', 'LONMAX', nbmcfc, k8b)
    call jeveuo(basefl//'.NUMO', 'L', knumo)
    write(chrefe,'(A8,A5,2I3.3,A5)') basefl(1:8),'.C01.',zi(knumo),&
     &                                 iv,'.REFE'
    call jeveuo(chrefe, 'L', kchref)
    mailla = zk24(kchref)(1:8)
    call dismoi('F', 'NB_NO_MAILLA', mailla, 'MAILLAGE', nbnoeu,&
                k8b, ie)
!
!
! 4.  RECUPERATION DU NOMBRE D'EQUATIONS DU MODELE
!     --------------------------------------------
    call jeveuo(nombm//'           .REFD', 'L', kdrif)
    matass = zk24(kdrif)(1:8)
    call dismoi('F', 'NOM_NUME_DDL', matass, 'MATR_ASSE', ib,&
                numddl, ie)
    call dismoi('F', 'NB_EQUA', matass, 'MATR_ASSE', neq,&
                k8b, ie)
!
!
! 5.  RECUPERATION DU NOMBRE DE MODE SUR BASE COMPLETE
!     ET SUR BASE REDUITE
!     -------------------
    call rsorac(nombm, 'LONUTI', ib, r8b, k8b,&
                c16b, 0.0d0, k8b, nbm0, 1,&
                nbtrou)
!     NBM0 = NBMCFC
!
    call getvis('SCHEMA_TEMPS', 'NB_MODE', 1, iarg, 1,&
                nbmode, n1)
    if (n1 .eq. 0) then
        nbmode = nbm0
    else if (nbmode.gt.nbm0) then
        nbmode = nbm0
        write(k4b,'(I4)') nbm0
        valk(1) = nombm
        valk(2) = k4b
        valk(3) = nombm
        call u2mesk('A', 'ALGORITH5_56', 3, valk)
    endif
!
    nbmd = nbmode
!
! 6.  RECUPERATION DES CARACTERISTIQUES MODALES
!     -----------------------------------------
! 6.1 CREATION DES OBJETS DE STOCKAGE
!
    call wkvect('&&MDITMI.PULSATIO', 'V V R8', nbmode, jpuls)
    call wkvect('&&MDITMI.MASSEGEN', 'V V R8', nbmode, jmasg)
    call wkvect('&&MDITMI.AMORTI', 'V V R8', nbmode, jamog)
    call wkvect('&&MDITMI.AMORTGEN', 'V V R8', nbmode, jamo1)
    call wkvect('&&MDITMI.BASEMODE', 'V V R8', nbmode*neq, jbase)
    call wkvect('&&MDITMI.LOCFL0', 'V V L', nbmode, jlocf)
!
! 6.2 PULSATIONS ET MASSES MODALES
!     STRUCTURE NON COUPLEE AVEC LE FLUIDE
!
    do 10 im = 1, nbmode
        call rsadpa(nombm, 'L', 1, 'OMEGA2', im,&
                    0, lomeg, k8b)
        zr(jpuls+im-1) = sqrt ( zr(lomeg) )
        call rsadpa(nombm, 'L', 1, 'MASS_GENE', im,&
                    0, lmasg, k8b)
        zr(jmasg+im-1) = zr(lmasg)
        zl(jlocf+im-1) = .false.
10  end do
!
! 6.3 AMORTISSEMENTS MODAUX
!     STRUCTURE NON COUPLEE AVEC LE FLUIDE
!
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', 1, iarg, 0,&
                r8b, n1)
    call getvid('AMOR_MODAL', 'LIST_AMOR', 1, iarg, 0,&
                k8b, n2)
    if ((n1.ne.0) .or. (n2.ne.0)) then
        if (n1 .ne. 0) then
            nbamor = -n1
        else
            call getvid('AMOR_MODAL', 'LIST_AMOR', 1, iarg, 0,&
                        listam, ib)
            call jelira(listam//'           .VALE', 'LONMAX', nbamor, k8b)
        endif
        if (nbamor .gt. nbmode) then
            vali (1) = nbmode
            vali (2) = nbamor
            vali (3) = nbmode
            valk (1) = 'PREMIERS COEFFICIENTS'
            call u2mesg('A', 'ALGORITH16_12', 1, valk, 3,&
                        vali, 0, 0.d0)
        endif
        if (nbamor .ge. nbmode) then
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', 1, iarg, nbmode,&
                            zr(jamog), ib)
            else
                call jeveuo(listam//'           .VALE', 'L', lamog)
                do 20 iam = 1, nbmode
                    zr(jamog+iam-1) = zr(lamog+iam-1)
20              continue
            endif
        else
            idiff = nbmode - nbamor
            vali (1) = idiff
            vali (2) = nbmode
            vali (3) = idiff
            call u2mesi('I', 'ALGORITH16_13', 3, vali)
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', 1, iarg, nbamor,&
                            zr(jamog), ib)
            else
                call jeveuo(listam//'           .VALE', 'L', lamog)
                do 30 iam = 1, nbamor
                    zr(jamog+iam-1) = zr(lamog+iam-1)
30              continue
            endif
            xamor = zr(jamog+nbamor-1)
            do 31 iam = nbamor+1, nbmode
                zr(jamog+iam-1) = xamor
31          continue
        endif
        do 40 im = 1, nbmode
            zr(jamo1+im-1) = 2.0d0 * zr(jamog+im-1) * zr(jmasg+im-1) * zr(jpuls+im-1)
40      continue
    endif
!
! 6.4 DEFORMEES MODALES
!
    call copmod(nombm, 'DEPL', neq, numddl, nbmode,&
                'R', zr(jbase), c16b)
!
! 6.5 RECUPERATION DE LA VITESSE D'ECOULEMENT DU FLUIDE
!
    call getvis('SCHEMA_TEMPS', 'NUME_VITE_FLUI', 1, iarg, 1,&
                numvif, n1)
    call jeveuo(basefl//'.VITE', 'L', kvite)
    vgap = zr(kvite+numvif-1)
!
! 6.6 PULSATIONS, MASSES ET AMORTISSEMENTS MODAUX
!     STRUCTURE COUPLEE AVEC LE FLUIDE A LA VITESSE D'ECOULEMENT CHOISIE
!
    call jeveuo(basefl//'.FREQ', 'L', kfreq)
    call jeveuo(basefl//'.MASG', 'L', kmasg)
    do 50 j = 1, nbmcfc
        im = zi(knumo+j-1)
        if (im .le. nbmode) then
            if (zr(kfreq+2*(j-1)+2*nbmcfc*(numvif-1)) .lt. 0.0d0) then
                write(k3iv,'(I3)') numvif
                write(k3im,'(I3)') im
                valk(1) = k3im
                valk(2) = k3iv
                call u2mesk('F', 'ALGORITH5_58', 2, valk)
            else
                zr(jpuls+im-1) = deuxpi * zr( kfreq+2*(j-1) +2*nbmcfc*( numvif-1))
            endif
            zr(jmasg+im-1) = zr(kmasg+j-1)
            zr(jamog+im-1) = zr(kfreq+2*(j-1)+2*nbmcfc*(numvif-1)+1)
            zr(jamo1+im-1) = 2.0d0 * zr(jamog+im-1) * zr(jmasg+im-1) * zr(jpuls+im-1)
            zl(jlocf+im-1) = .true.
        endif
50  end do
!
!
! 7.  RECUPERATION DES CARACTERISTIQUES DE LA CONFIGURATION ETUDIEE
!     -------------------------------------------------------------
! 7.1 CREATION DES OBJETS DE STOCKAGE
!
    call wkvect('&&MDITMI.NUOR', 'V V I', nbmode, jnuor)
    do 60 j = 1, nbmode
        zi(jnuor+j-1) = j
60  end do
!
    if (itypfl .eq. 1) then
        call wkvect('&&MDITMI.TEMP.IRES', 'V V I', nbnoeu, lires)
        call wkvect('&&MDITMI.TEMP.PROFV', 'V V R8', 2*nbnoeu+1, lprofv)
        call wkvect('&&MDITMI.TEMP.RHOE', 'V V R8', 2*nbnoeu, jrhoe)
        call wkvect('&&MDITMI.TEMP.BASEFL', 'V V R8', nbmode*nbnoeu, jbasf)
        call wkvect('&&MDITMI.TEMP.PHIE', 'V V R8', 2, jphie)
        call wkvect('&&MDITMI.TEMP.ABSCV', 'V V R8', nbnoeu, jabsc)
        iveci1 = lires
        ivecr1 = lprofv
        ivecr2 = jrhoe
        ivecr3 = jbasf
        ivecr4 = jphie
        ivecr5 = jabsc
    else if (itypfl.eq.2) then
        call wkvect('&&MDITMI.TEMP.CODIM', 'V V R8', 4, jcodim)
        call wkvect('&&MDITMI.TEMP.POIDS', 'V V R8', 2*nbmode, jpoids)
        call wkvect('&&MDITMI.TEMP.PHIE', 'V V R8', 1, jphie)
        iveci1= 1
        ivecr1= jmasg
        ivecr2 = jcodim
        ivecr3 = jpoids
        ivecr4 = jphie
        ivecr5= 1
    endif
!
! 7.2 RECUPERATION DES CARACTERISTIQUES
!
    call mdconf(typflu, nombm, mailla, nbmode, nbnoeu,&
                zi(jnuor), 1, indic, zi(iveci1), zr(ivecr1),&
                zr(ivecr2), zr(ivecr3), zr(ivecr4), zr(ivecr5))
!
!
! 8.  RECUPERATION DES OPTIONS DE CALCUL
!     ----------------------------------
! 8.1 CALCUL OU NON D'UN TRANSITOIRE
!
    itrans = 0
    call getvtx('SCHEMA_TEMPS', 'ETAT_STAT', 1, iarg, 1,&
                ouinon, n1)
    call getvr8('SCHEMA_TEMPS', 'PREC_DUREE', 1, iarg, 1,&
                eps, n1)
    call getvr8('SCHEMA_TEMPS', 'TS_REG_ETAB', 1, iarg, 1,&
                ts, nts)
    if (ouinon .eq. 'OUI') itrans = 1
!
! 8.2 PRISE EN COMPTE OU NON DU SAUT DE FORCE FLUIDELASTIQUE
!     D'AMORTISSEMENT AU COURS DES PHASES DE CHOC
!
    icoupl = 0
    call getvtx('SCHEMA_TEMPS', 'CHOC_FLUI', 1, iarg, 1,&
                ouinon, n1)
    call getvis('SCHEMA_TEMPS', 'NB_MODE_FLUI', 1, iarg, 1,&
                nbmp, nmp)
    if (ouinon .eq. 'OUI') icoupl = 1
    if (nbmp .eq. 0) icoupl = 0
!
    if (nmp .eq. 0) then
        nbmp = nbmcfc
        if (icoupl .eq. 1) then
            write(k4b,'(I4)') nbmp
            valk(1) = k4b
            valk(2) = basefl(1:8)
            call u2mesk('A', 'ALGORITH5_59', 2, valk)
        endif
    else if (nbmp.gt.nbmcfc) then
        nbmp = nbmcfc
        if (icoupl .eq. 1) then
            write(k4b,'(I4)') nbmp
            valk(1) = basefl(1:8)
            valk(2) = k4b
            call u2mesk('A', 'ALGORITH5_60', 2, valk)
        endif
    endif
!
    call jedema()
!
! --- FIN DE MDITMI.
end subroutine
