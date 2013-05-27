subroutine deltau(jrwork, jnbpg, nbpgt, nbordr, ordini,&
                  nmaini, nbmap, numpaq, tspaq, nommet,&
                  nomcri, nomfor, grdvie, forvie, cesr)
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
! person_in_charge: jean.angles at edf.fr
! TOLE  CRP_20
    implicit     none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterc/loisem.h'
    include 'asterc/lor8em.h'
    include 'asterc/r8pi.h'
    include 'asterfort/acgrdo.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedisp.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jerazo.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rcpare.h'
    include 'asterfort/rnomat.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vecnuv.h'
    include 'asterfort/wkvect.h'
    integer :: jrwork, jnbpg, nbpgt, nbordr, nmaini, numpaq, nbmap
    integer :: tspaq, ordini
    character(len=8) :: grdvie
    character(len=16) :: nomcri, nommet, nomfor, forvie
    character(len=19) :: cesr
! ---------------------------------------------------------------------
! BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
!      POUR CHAQUE POINT DE GAUSS D'UN <<PAQUET>> DE MAILLES.
! ---------------------------------------------------------------------
! ARGUMENTS:
! JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                       DU <<PAQUET>> DE MAILLES.
! JNBPG      IN    I  : ADRESSE DU VECTEUR CONTENANT LE NOMBRE DE
!                       POINT DE GAUSS DE CHAQUE MAILLE DU MAILLAGE.
! NBPGT      IN    I  : NOMBRE TOTAL DE POINTS DE GAUSS A TRAITER.
! NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                       STRUCTURE DE DONNEES RESULTAT.
! ORDINI     IN    I  : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
! NMAINI     IN    I  : NUMERO DE LA 1ERE MAILLE DU <<PAQUET>> DE
!                       MAILLES COURANT.
! NBMAP      IN    I  : NOMBRE DE MAILLES DANS LE <<PAQUET>> DE
!                       MAILLES COURANT.
! NUMPAQ     IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
! TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                       COURANT.
! NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
!                       CIRCONSCRIT.
! NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
! CESR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
!                       RESULTATS.
!
! REMARQUE :
!  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
!    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
!-----------------------------------------------------------------------
!
    integer :: j, kwork, n, jcerd, jcerl, jcerv, jad
    integer :: iret, imap, icesd, icesl, icesv, ibid
    integer :: ipg, tneces, tdisp, jvecpg, jvectn
    integer :: jvectu, jvectv, ngam, dim, tab2(18)
    integer :: nbpg, sompgw, nbpgp, l, jdtaum, jresun
    integer :: icmp, vali(2)
    real(kind=8) :: dgam, gamma, pi, dphi, tab1(18)
    real(kind=8) :: phi0, vala, valb, coefpa, vresu2(24), valpar(22)
    integer :: icodwo, iarg
    character(len=8) :: chmat1, nommat
    character(len=10) :: optio
    character(len=19) :: chmat, cesmat
!
!
!-----------------------------------------------------------------------
!234567                                                              012
!-----------------------------------------------------------------------
    data  tab1/ 180.0d0, 60.0d0, 30.0d0, 20.0d0, 15.0d0, 12.857d0,&
     &             11.25d0, 10.588d0, 10.0d0, 10.0d0, 10.0d0, 10.588d0,&
     &             11.25d0, 12.857d0, 15.0d0, 20.0d0, 30.0d0, 60.0d0 /
!
    data  tab2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,&
     &           12, 9, 6, 3 /
!
    pi = r8pi()
!-----------------------------------------------------------------------
!
    call jemarq()
!
! CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
! CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
!              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX
!
    call wkvect('&&DELTAU.DTAU_MAX', 'V V R', 209, jdtaum)
    call wkvect('&&DELTAU.RESU_N', 'V V I', 209, jresun)
!
! CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
! CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
!
    call wkvect('&&DELTAU.VECT_NORMA', 'V V R', 630, jvectn)
    call wkvect('&&DELTAU.VECT_TANGU', 'V V R', 630, jvectu)
    call wkvect('&&DELTAU.VECT_TANGV', 'V V R', 630, jvectv)
!
! OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
! DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....
!
    call jeveuo(cesr//'.CESD', 'L', jcerd)
    call jeveuo(cesr//'.CESL', 'E', jcerl)
    call jeveuo(cesr//'.CESV', 'E', jcerv)
!
!
! RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
!
    call getvid(' ', 'CHAM_MATER', 1, iarg, 1,&
                chmat1, iret)
    chmat = chmat1//'.CHAMP_MAT'
    cesmat = '&&DELTAU.CESMAT'
    call carces(chmat, 'ELEM', ' ', 'V', cesmat,&
                'A', iret)
    call jeveuo(cesmat//'.CESD', 'L', icesd)
    call jeveuo(cesmat//'.CESL', 'L', icesl)
    call jeveuo(cesmat//'.CESV', 'L', icesv)
!
    tneces = 209*nbordr*2
    call jedisp(1, tdisp)
    tdisp = (tdisp * loisem()) / lor8em()
    if (tdisp .lt. tneces) then
        vali (1) = tdisp
        vali (2) = tneces
        call u2mesg('F', 'PREPOST5_8', 0, ' ', 2,&
                    vali, 0, 0.d0)
    else
        call wkvect('&&DELTAU.VECTPG', 'V V R', tneces, jvecpg)
        call jerazo('&&DELTAU.VECTPG', tneces, 1)
    endif
!
    dgam = 10.0d0
!
    n = 0
    dim = 627
    do 300 j = 1, 18
        gamma=(j-1)*dgam*(pi/180.0d0)
        dphi=tab1(j)*(pi/180.0d0)
        ngam=tab2(j)
        phi0=dphi/2.0d0
!
        call vecnuv(1, ngam, gamma, phi0, dphi,&
                    n, 1, dim, zr(jvectn), zr(jvectu),&
                    zr(jvectv))
!
300  end do
!
! CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
! POINT DE GAUSS DU PAQUET DE MAILLES.
    l = 1
    nbpg = 0
    nbpgp = 0
    kwork = 0
    sompgw = 0
!
    do 400 imap = nmaini, nmaini+(nbmap-1)
        if (imap .gt. nmaini) then
            kwork = 1
            sompgw = sompgw + zi(jnbpg + imap-2)
        endif
        nbpg = zi(jnbpg + imap-1)
! SI LA MAILLE COURANTE N'A PAS DE POINTS DE GAUSS, LE PROGRAMME
! PASSE DIRECTEMENT A LA MAILLE SUIVANTE.
        if (nbpg .eq. 0) then
            goto 400
        endif
!
        nbpgp = nbpgp + nbpg
        if ((l*int(nbpgt/10.0d0)) .lt. nbpgp) then
            write(6,*)numpaq,'   ',(nbpgp-nbpg)
            l = l + 1
        endif
!
! RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE
! ET DES PARAMETRES ASSOCIES AU CRITERE CHOISI POUR LA MAILLE COURANTE.
!
        optio = 'DOMA_ELGA'
        call rnomat(icesd, icesl, icesv, imap, nomcri,&
                    ibid, ibid, ibid, optio, vala,&
                    valb, coefpa, nommat)
!
        call rcpare(nommat, 'FATIGUE', 'WOHLER', icodwo)
        if (icodwo .eq. 1) then
            call u2mesk('F', 'FATIGUE1_90', 1, nomcri(1:16))
        endif
!
!
        do 420 ipg = 1, nbpg
!
            call jerazo('&&DELTAU.VECTPG', tneces, 1)
!
! REMPACER PAR ACMATA
            call acgrdo(jvectn, jvectu, jvectv, nbordr, ordini,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        jvecpg, jdtaum, jresun, nommet, nommat,&
                        nomcri, vala, coefpa, nomfor, grdvie,&
                        forvie, valpar, vresu2)
!
!
! C AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE
!
            do 550 icmp = 1, 24
                call cesexi('C', jcerd, jcerl, imap, ipg,&
                            1, icmp, jad)
!
!              -- TOUTES LES MAILLES NE SAVENT PAS CALCULER LA FATIGUE :
                if (jad .eq. 0) then
                    call assert(icmp.eq.1)
                    call assert(ipg.eq.1)
                    goto 400
                endif
                jad = abs(jad)
                zl(jcerl - 1 + jad) = .true.
                zr(jcerv - 1 + jad) = vresu2(icmp)
!
550          continue
!
420      continue
400  end do
!
! MENAGE
!
    call detrsd('CHAM_ELEM_S', cesmat)
!
    call jedetr('&&DELTAU.DTAU_MAX')
    call jedetr('&&DELTAU.RESU_N')
    call jedetr('&&DELTAU.VECT_NORMA')
    call jedetr('&&DELTAU.VECT_TANGU')
    call jedetr('&&DELTAU.VECT_TANGV')
!
    call jedetr('&&DELTAU.VECTPG')
!
    call jedema()
end subroutine
