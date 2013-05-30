subroutine avcrit(nbvec, nbordr, vectn, vwork, tdisp,&
                  kwork, sommw, tspaq, i, vala,&
                  coefpa, ncycl, vmin, vmax, omin,&
                  omax, nomcri, nomfor, gdreq)
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
! TOLE CRP_21 CRS_1404
! person_in_charge: jean.angles at edf.fr
    implicit      none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterfort/anacri.h'
    include 'asterfort/aveppr.h'
    include 'asterfort/aveteq.h'
    include 'asterfort/avetpr.h'
    include 'asterfort/avphyd.h'
    include 'asterfort/avsieq.h'
    include 'asterfort/avsign.h'
    include 'asterfort/avsipr.h'
    include 'asterfort/fointe.h'
    include 'asterfort/fonbpa.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nbvec, nbordr, ncycl(nbvec)
    integer :: omin(nbvec*(nbordr+2)), omax(nbvec*(nbordr+2))
    integer :: tdisp, kwork, sommw, tspaq, i
    real(kind=8) :: vectn(3*nbvec)
    real(kind=8) :: vwork(tdisp)
    real(kind=8) :: vala, coefpa
    real(kind=8) :: vmin(nbvec*(nbordr+2)), vmax(nbvec*(nbordr+2))
    real(kind=8) :: gdreq(nbvec*nbordr)
    character(len=16) :: nomcri, nomfor
! ----------------------------------------------------------------------
! BUT: CALCULER LA CONTRAINTE EQUIVALENTE POUR TOUS LES VECTEURS NORMAUX
!      A TOUS LES NUMEROS D'ORDRE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
!  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
!  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
!  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
!  VMIN     IN   R  : VALEURS MIN DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
!  VMAX     IN   R  : VALEURS MAX DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
!  OMIN     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MIN DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
!  OMAX     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MAX DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
!  GDREQ    OUT  R  : VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
!                     EQUIVALENTE, POUR TOUS LES NUMEROS D'ORDRE
!                     DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
    integer :: ivect, ad0, ad1, ad2, icycl, nval, ipar, j, np
    integer :: ibid, nparma, jprof, paract(30), iarg
    real(kind=8) :: coepre, valpar(30), valpu(30)
    real(kind=8) :: vsign(nbvec*nbordr), vphydr(nbordr)
    real(kind=8) :: vsipr(nbordr), vepsn(nbordr)
    real(kind=8) :: vetpr(nbordr), vsitn(nbordr)
    real(kind=8) :: veppr(nbordr), vsipn(nbordr)
    real(kind=8) :: vsieq(nbordr), veteq(nbordr)
    logical :: fordef, lbid
    character(len=8) :: nompf(30), nompar(30)
    character(len=16) :: typcha
    character(len=24) :: chnom, cbid
!  VSIGN    IN   R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
!                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
!                     DE CHAQUE VECTEUR NORMAL, ON UTILISE
!                     VSIGN UNIQUEMENT DANS LE CRITERE MATAKE_MODI_AV.
!  VPHYDR   IN   R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
!                     TOUS LES INSTANTS, ON UTILISE VPHYDR
!                     UNIQUEMENT DANS LE CRITERE DE DANG VAN.
!     ------------------------------------------------------------------
    data  nompar /   'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',&
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2',&
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',&
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',&
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',&
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',&
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',&
     &                 'SITRE_1','SITRE_2'     /
!-----------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
!
! RECUPERER LA LISTE DE GRANDEURS ACTIVES
!
    typcha = 'NON_PERIODIQUE'
!
    call anacri(nomcri, nomfor, typcha, 'NON', paract,&
                fordef, lbid, lbid, lbid, lbid)
!
! VOIR SI LE CRITERE DU TYPE DE PLANE CRITIQUE
! SI OUI, ON TOURNE NVVECT, SI NON ON NVEC=1
!
!       PLACRI = .FALSE.
!       DO 30 J = 1, 8
!          IF (PARACT(J) .EQ. 1) THEN
!             PLACRI = .TRUE.
!             GOTO 31
!          ENDIF
!
! 30    CONTINUE
!
! 31    CONTINUE
!
!-----------------------------------------------------------------------
! CALCULER LES GRANDEURS
!----------------------------------------------------------------------
! 1.1 CALCUL DE LA CONTRAINTE NORMALE
    if ((paract(3) .eq. 1 ) .or. (paract(4) .eq. 1 )) then
        call avsign(nbvec, nbordr, vectn, vwork, tdisp,&
                    kwork, sommw, tspaq, i, vsign)
!
    endif
!
! 1.2 CALCUL DE LA PRESSION HYDROSTATIQUE
    if ((paract(5) .eq. 1 ) .or. (paract(6) .eq. 1 )) then
!
        call avphyd(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, vphydr)
!
    endif
!
    if ((paract(9) .eq. 1 ) .or. (paract(10) .eq. 1 ) .or. (paract(11) .eq. 1 ) .or.&
        (paract(12) .eq. 1 )) then
!
        call avsipr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, vsipr, vepsn)
!
    endif
!
!
    if ((paract(13) .eq. 1 ) .or. (paract(14) .eq. 1 ) .or. (paract(15) .eq. 1 ) .or.&
        (paract(16) .eq. 1 )) then
!
        call avetpr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, vetpr, vsitn)
!
    endif
!
    if ((paract(17) .eq. 1 ) .or. (paract(18) .eq. 1 ) .or. (paract(19) .eq. 1 ) .or.&
        (paract(20) .eq. 1 )) then
!
        call aveppr(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, veppr, vsipn)
!
    endif
!
    if ((paract(21) .eq. 1 ) .or. (paract(22) .eq. 1 )) then
!
        call avsieq(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, vsieq)
!
    endif
!
    if ((paract(23) .eq. 1 ) .or. (paract(24) .eq. 1 )) then
!
        call aveteq(nbordr, vwork, tdisp, kwork, sommw,&
                    tspaq, i, veteq)
!
    endif
!----------------------------------------------------------------------
! EVALUER LES GRANDEURS
!----------------------------------------------------------------------
! 1. CRITERE DE DANG_VAN MODIFIE (AMPLITUDE VARIABLE)
!      IF (.NOT. POST) THEN
    call getvr8(' ', 'COEF_PREECROU', 1, iarg, 1,&
                coepre, nval)
!      ENDIF
!
    if (nomcri(1:7) .eq. 'FORMULE') then
! NOMBRE DE PARAMETRES DISPONIBLES
        nparma = 30
! RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
        chnom(20:24) = '.PROL'
        chnom(1:19) = nomfor
!
        call jeveuo(chnom, 'L', jprof)
        call fonbpa(nomfor, zk24(jprof), cbid, nparma, np,&
                    nompf)
    endif
!
    do 10 ivect = 1, nbvec
        ad0 = (ivect-1)*nbordr
        do 20 icycl = 1, ncycl(ivect)
            ad1 = (ivect-1)*nbordr + icycl
            ad2 = (ivect-1)*(nbordr+2) + icycl
!
            if (nomcri(1:14) .eq. 'MATAKE_MODI_AV') then
                gdreq(ad1)= coepre*abs((vmax(ad2) - vmin(ad2))/2.0d0)&
                + vala*max(vsign(ad0+omax(ad2)), vsign(ad0+omin(ad2)),&
                0.0d0)
                gdreq(ad1)= gdreq(ad1)*coefpa
!
            endif
!
            if (nomcri(1:16) .eq. 'DANG_VAN_MODI_AV') then
                gdreq(ad1)= coepre*abs((vmax(ad2) - vmin(ad2))/2.0d0)&
                + vala*max(vphydr(omax(ad2)), vphydr(omin(ad2)),0.0d0)
!
                gdreq(ad1)= gdreq(ad1)*coefpa
            endif
!
            if (nomcri(1:16) .eq. 'FATESOCI_MODI_AV') then
                gdreq(ad1)= coepre*abs((vmax(ad2) - vmin(ad2))/2.0d0)*&
                (1.0d0 + vala*max(vsign(ad0+omax(ad2)), vsign(ad0+&
                omin(ad2)),0.0d0))
                gdreq(ad1)= gdreq(ad1)*coefpa
            endif
!
            if (nomcri(1:7) .eq. 'FORMULE') then
                valpar(1) = vmax(ad2)
                valpar(2) = vmin(ad2)
                valpar(3) = vsign(ad0+omax(ad2))
                valpar(4) = vsign(ad0+omin(ad2))
                valpar(5) = vphydr(omax(ad2))
                valpar(6) = vphydr(omin(ad2))
                valpar(7) = vmax(ad2)
                valpar(8) = vmin(ad2)
                valpar(9) = vsipr(omax(ad2))
                valpar(10) = vsipr(omin(ad2))
                valpar(11) = vepsn(omax(ad2))
                valpar(12) = vepsn(omin(ad2))
                valpar(13) = vetpr(omax(ad2))
                valpar(14) = vetpr(omin(ad2))
                valpar(15) = vsitn(omax(ad2))
                valpar(16) = vsitn(omin(ad2))
                valpar(17) = veppr(omax(ad2))
                valpar(18) = veppr(omin(ad2))
                valpar(19) = vsipn(omax(ad2))
                valpar(20) = vsipn(omin(ad2))
                valpar(21) = vsieq(omax(ad2))
                valpar(22) = vsieq(omin(ad2))
                valpar(23) = veteq(omax(ad2))
                valpar(24) = veteq(omin(ad2))
                valpar(25) = 0.d0
                valpar(26) = 0.d0
                valpar(27) = 0.d0
                valpar(28) = 0.d0
                valpar(29) = 0.d0
                valpar(30) = 0.d0
!
                do 75 j = 1, np
                    do 65 ipar = 1, nparma
                        if (nompf(j) .eq. nompar(ipar)) then
                            valpu(j) = valpar(ipar)
                            goto 75
                        endif
65                  continue
75              continue
!
                call fointe('F', nomfor, np, nompf, valpu,&
                            gdreq(ad1), ibid)
!
            endif
!
20      continue
!
!         IF ( .NOT. PLACRI) THEN
!            GOTO 99
!         ENDIF
!
10  end do
!
    call jedema()
!
end subroutine
