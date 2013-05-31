subroutine te0382(option, nomte)
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!         CALCUL DE L'INDICATEUR D'ERREUR SUR UN ELEMENT 2D
!         POUR L'ELEMENT XFEM COURANT AVEC LA METHODE
!         DES RESIDUS EXPLICITES.
!         OPTION : 'ERME_ELEM'
!
! REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
!            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
!
! ......................................................................
! CORPS DU PROGRAMME
! aslint: disable=W1501
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/calnor.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/ermeb2.h'
    include 'asterfort/ermes2.h'
    include 'asterfort/ermev2.h'
    include 'asterfort/infniv.h'
    include 'asterfort/intenc.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/teattr.h'
    include 'asterfort/tecach.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/uthk.h'
    include 'asterfort/utjac.h'
    include 'asterfort/xrmes2.h'
    include 'asterfort/xrmev2.h'
    character(len=16) :: option, nomte
!
!
!
    integer :: nbnase, nbnamx
!     SOUS-ELEMENTS TOUJOURS LINEAIRES => ON A TOUJOURS NBNASE=2
    parameter (nbnase=2)
!     ON DIMENSIONNE LES VECTEURS POUR LE NBRE MAX DE NOEUDS PAR ARETE
!     (CAS D'1 ELEMENT PARENT QUADRATIQUE) => NBNAMX=3
    parameter (nbnamx=3)
!
    integer :: ifm, niv
    integer :: iadzi, iazk24
    integer :: ibid, iaux, iret, itab(7), itabid(9, 6, 4)
    integer :: igeom, jtime
    integer :: ierr, ivois
    integer :: imate
    integer :: iref1, iref2
    integer :: ndim
    integer :: nno, npg, idfde, jgano
    integer :: nbcmp
    integer :: tyv
    integer :: npgp, nnop, nnosp, ipoidp, ivfp
    integer :: isigno
    integer :: nbs, idfse
    integer :: inp
    integer :: ino, nbnapa
    integer :: jpintt, jcnset, jlonch, jvoxse, jsigse, jpmilt
    integer :: nse, ise, in, j, ipg
    integer :: levois
    integer :: irese, kpg, spt
!
    real(kind=8) :: r8bid, rtbid3(3)
    real(kind=8) :: dfdxp(9), dfdyp(9), poidp, he, hse, hf, coeff
    real(kind=8) :: sg11(nbnamx), sg22(nbnamx), sg12(nbnamx), jaco(nbnamx)
    real(kind=8) :: nx(nbnamx), ny(nbnamx), tx(nbnamx), ty(nbnamx)
    real(kind=8) :: chx(nbnamx), chy(nbnamx)
    real(kind=8) :: inst, inte, orien
    real(kind=8) :: sig11(nbnamx), sig22(nbnamx), sig12(nbnamx)
    real(kind=8) :: tvol, tvolse, tsau, tnor, nor, norsig, sigcal
    real(kind=8) :: e, nu, valres(2), r8tmp, coorse(81)
!
    integer :: icodre(2)
    character(len=2) :: noeu
    character(len=3) :: typnor
    character(len=8) :: typmav, elrefe, famil, poum
    character(len=8) :: fami(6), elrese(6)
    character(len=8) :: nompar(2)
    character(len=8) :: enr
    character(len=16) :: phenom, blan16, nomtse
    character(len=24) :: valk(2)
!
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
! ----------------------------------------------------------------------
! ----- NORME CALCULEE : SEMI-H1 (H1) OU ENERGIE (NRJ) -----------------
! ----------------------------------------------------------------------
!
    data typnor / 'NRJ' /
!
! ----------------------------------------------------------------------
! 1 -------------- GESTION DES DONNEES ---------------------------------
! ----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
! 1.1. --- LES INCONTOURNABLES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVOISIN', 'L', ivois)
    call jevech('PTEMPSR', 'L', jtime)
    inst=zr(jtime-1+1)
!
    call jevech('PERREUR', 'E', ierr)
!
! 1.2. --- LES CARACTERISTIQUES DE LA MAILLE EN COURS
    call tecael(iadzi, iazk24)
    valk(1)=zk24(iazk24-1+3)
    valk(2)=option
!
    call elref1(elrefe)
!
    if (niv .ge. 2) then
        write(ifm,*) ' '
        write(ifm,*) '================================================='
        write(ifm,*) ' '
        write(ifm,*) 'MAILLE NUMERO', zi(iadzi),', DE TYPE ', elrefe
    endif
!
! --- ELEMENT PARENT DE REFERENCE : RECUP DE NNO, NPG ET IDFDE
!
    call elref4(' ', 'RIGI', ndim, nnop, nnosp,&
                npgp, ipoidp, ivfp, idfde, jgano)
!
!     !!! ATTENTION !!! LA VALEUR DE NOMTSE EST UTILISEE POUR DEFINIR
!     LE "TYPE" DU SOUS-ELEMENT AFIN DE CALCULER SA TAILLE AVEC LA
!     ROUTINE UTHK(), DANS LAQUELLE UN TEST EST REALISE SUR NOMTSE(5:6)
!     2D => SOUS ELEMENTS SONT DES TRIANGLES
!             0123456789012345
    blan16='                '
    nomtse=blan16
!                   123456
    nomtse(1:6)= '    TR'
!
! --- SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG ET IDFSE
!
    if (.not.iselli(elrefe) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
!
    call elref4(elrese(ndim+irese), fami(ndim+irese), ibid, nno, ibid,&
                npg, ibid, ibid, idfse, ibid)
!
! --- RECUPERATION DES CHAMPS IN "CLASSIQUES"
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PFORCE', 'L', iref1)
    call jevech('PPRESS', 'L', iref2)
    call tecach('OOO', 'PCONTNO', 'L', 3, itab,&
                iret)
    isigno=itab(1)
    nbcmp=nbsigm()
!
! --- RECUPERATION DES CHAMPS IN "XFEM"
!
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PCVOISX', 'L', jvoxse)
    call jevech('PCONTSER', 'L', jsigse)
!     PROPRE AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if (ibid .eq. 0 .and. (enr.eq.'XH'.or.enr.eq.'XHC') .and. ndim .le. 2) call jevech(&
                                                                           'PPMILTO', 'L',&
                                                                           jpmilt)
!
! ----------------------------------------------------------------------
! ----------------------------- PREALABLES -----------------------------
! ----------------------------------------------------------------------
!
! --- INITIALISATION DES TERMES VOLUMIQUE, DE SAUT ET NORMAL
!
    tvol=0.d0
    tsau=0.d0
    tnor=0.d0
!
! --- CALCUL DU DIAMETRE DE L'ELEMENT PARENT
!
    call uthk(nomte, zr(igeom), he, ndim, itab,&
              ibid, ibid, ibid, niv, ifm)
!
! --- CALCUL DE LA NORME DE SIGMA
!
    norsig = 0.d0
!
    do 100 , ipg=1,npgp
!
! ----- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X ET /Y
!
    call dfdm2d(nno, ipg, ipoidp, idfde, zr(igeom),&
                dfdxp, dfdyp, poidp)
!
! ----- CALCUL DE LA DIVERGENCE (INUTILISEE ICI) ET DE LANORME DE SIGMA
!
    iaux=ivfp+(ipg-1)*nnop
    ibid = 1
    call ermev2(nnop, igeom, zr(iaux), zr(isigno), nbcmp,&
                dfdxp, dfdyp, poidp, ibid, r8bid,&
                r8bid, nor)
!
    norsig=norsig+nor*poidp
!
    100 end do
!
! ----------------------------------------------------------------------
! ------------ BOUCLE SUR LES COTES DE L'ELEMENT PARENT : --------------
! ------------ CALCUL DU TERME DE BORD SUR CHAQUE COTE    --------------
! ----------------------------------------------------------------------
!
!     NBS : NOMBRE DE NOEUDS SOMMETS PAR ARETE POUR L'ELEMENT PARENT
    if (elrefe(1:2) .eq. 'TR') then
        nbs=3
    else
        nbs=4
    endif
!
!     NBNAPA : NBRE DE NOEUDS PAR ARETE POUR L'ELEMENT PARENT
    noeu=elrefe(3:3)
    if (noeu .eq. '3' .or. noeu .eq. '4') then
        nbnapa=2
    else
        nbnapa=3
    endif
!
! --- CALCUL DE L'ORIENTATION DE LA MAILLE
!
    call utjac(.true., zr(igeom), 1, idfde, 0,&
               ibid, nnop, orien)
!
! --- BOUCLE SUR LES COTES DU PARENT
!
    do 200 , inp=1,nbs
!
! ----- TYPE DU DU VOISIN
!
    tyv=zi(ivois+7+inp)
!
    if (tyv .ne. 0) then
!
! ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
!
        call jenuno(jexnum('&CATA.TM.NOMTM', tyv), typmav)
!
! ------- CALCUL DE : NORMALE, TANGENTE, ET JACOBIEN
!
        iaux = inp
        call calnor('2D', zr(igeom), iaux, nbs, nbnapa,&
                    orien, ibid, ibid, itabid, ibid,&
                    ibid, ibid, jaco, nx, ny,&
                    rtbid3, tx, ty, hf)
!
! ------- SI L'ARETE N'EST PAS SUR LA FRONTIERE DE LA STRUCTURE...
! ------- ON CALCULE LE TERME DE SAUT POUR LES ELEMENTS PARENTS
!
        if (typmav(1:4) .eq. 'TRIA' .or. typmav(1:4) .eq. 'QUAD') then
!
! --------- CALCUL DU SAUT DE CONTRAINTES
!
            call ermes2(inp, elrefe, typmav, iref1, ivois,&
                        isigno, nbcmp, sg11, sg22, sg12)
!
! --------- CALCUL DE L'INTEGRALE SUR LE BORD
!
            call r8inir(nbnamx, 0.d0, chx, 1)
            call r8inir(nbnamx, 0.d0, chy, 1)
            call intenc(nbnapa, jaco, chx, chy, sg11,&
                        sg22, sg12, nx, ny, inte)
!
! --------- ACTUALISATION DU TERME DE BORD
!
            if (typnor .eq. 'NRJ') then
                tsau=tsau+0.5d0*hf*inte
            else
                tsau=tsau+0.5d0*sqrt(hf)*sqrt(inte)
            endif
!
! ------- SI L'ARETE EST SUR LA FRONTIERE DE LA STRUCTURE...
! ------- ON CALCULE LE TERME NORMAL POUR LES ELEMENTS PARENTS
!
        else if (typmav(1:2).eq.'SE') then
!
! --------- CALCUL DES EFFORTS SURFACIQUES ET CONTRAINTES SI NEUMANN,
! --------- SINON -> EFFORTS=0 (ERMEB2)
!
            call ermeb2(inp, iref1, iref2, ivois, igeom,&
                        isigno, elrefe, nbcmp, inst, nx,&
                        ny, tx, ty, sig11, sig22,&
                        sig12, chx, chy)
!
! --------- CALCUL DE L'INTEGRALE DE BORD
!
            call intenc(nbnapa, jaco, chx, chy, sig11,&
                        sig22, sig12, nx, ny, inte)
!
! --------- ACTUALISATION DU TERME DE BORD
!
            if (typnor .eq. 'NRJ') then
                tnor=tnor+hf*inte
            else
                tnor=tnor+sqrt(hf)*sqrt(inte)
            endif
!
! ----------------------------------------------------------------------
! --------------- CURIEUX ----------------------------------------------
! ----------------------------------------------------------------------
!
        else
!
            valk(1)=typmav(1:4)
            call u2mesk('F', 'INDICATEUR_10', 1, valk)
!
        endif
!
    endif
!
    200 end do
!
! ----------------------------------------------------------------------
! ---------- FIN BOUCLE SUR LES COTES DE L'ELEMENT PARENT  -------------
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ------------------- BOUCLE SUR LES SOUS ELEMENTS ---------------------
! ----------------------------------------------------------------------
!
! --- RECUPERATION DU DECOUPAGE EN NSE SIMPLEXES
!
    nse=zi(jlonch-1+1)
!
    do 310 ise = 1, nse
!
! ----- BOUCLE SUR LES 3 SOMMETS DU SOUS-ELEMENT
!
        do 311 in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do 312 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
312          continue
311      continue
!
! ----- CALCUL DE LA TAILLE DU SOUS-ELEMENT
!
        call uthk(nomtse, coorse, hse, ndim, itab,&
                  ibid, ibid, ibid, niv, ifm)
!
! ----- CALCUL DE L'ORIENTATION DU SOUS-ELEMENT
!
        call utjac(.true., coorse, 1, idfse, 0,&
                   ibid, nno, orien)
!
! ----------------- CALCUL DU TERME VOLUMIQUE -----------------------
!
        tvolse=0.d0
        call xrmev2(ise, npg, ndim, igeom, jsigse,&
                    coorse, tvolse)
!
        if (typnor .eq. 'NRJ') then
            tvol=tvol+tvolse*hse**2
        else
            tvol=tvol+sqrt(tvolse)*hse
        endif
!
! --------------------------------------------------------------------
! ------------- BOUCLE SUR LES ARETES DU SOUS-ELEMENT ----------------
! --------------------------------------------------------------------
!
        do 314 in = 1, nno
!
            levois=zi(jvoxse-1+nno*(ise-1)+in)
!
! ------- PRESENCE OU NON D'UN VOISIN DE L'AUTRE COTE DE L'ARETE
!
            if (levois .ne. 0) then
!
! --------- CALCUL DE NORMALES, TANGENTES ET JACOBIENS
!
                iaux = in
                call calnor('2D', coorse, iaux, nno, nbnase,&
                            orien, ibid, ibid, itabid, ibid,&
                            ibid, ibid, jaco, nx, ny,&
                            rtbid3, tx, ty, hf)
!
! --------- CALCUL DU SAUT DE CONTRAINTES AUX NOEUDS S-E/VOISIN
! --------- (EQUIVALENT XFEM DE ERMES2)
!
                call xrmes2(ndim, nbnase, ise, in, levois,&
                            jsigse, nno, nbcmp, jcnset, sg11,&
                            sg22, sg12)
!
! --------- CALCUL DE L'INTEGRALE SUR L'ARETE
!
                call r8inir(nbnase, 0.d0, chx, 1)
                call r8inir(nbnase, 0.d0, chy, 1)
!           ATTENTION, NBNASE=2 ALORS QUE DS INTENC TOUS LES ARGUMENTS
!           D'ENTREE SONT DIMENSIONNES A 3, MAIS CA NA POSE PAS DE PB
                call intenc(nbnase, jaco, chx, chy, sg11,&
                            sg22, sg12, nx, ny, inte)
!
! --------- ACTUALISATION DU TERME DE BORD
!
                if (typnor .eq. 'NRJ') then
                    tsau=tsau+0.5d0*hf*inte
                else
                    tsau=tsau+0.5d0*sqrt(hf)*sqrt(inte)
                endif
!
            endif
!
314      continue
!
! --------------------------------------------------------------------
! --------- FIN BOUCLE SUR LES ARETES DU SOUS-ELEMENT ----------------
! --------------------------------------------------------------------
!
310  end do
!
!
! ----------------------------------------------------------------------
! ------------------ FIN BOUCLE SUR LES SOUS ELEMENTS  -----------------
! ----------------------------------------------------------------------
!
!     ATTENTION, NBNAPA=2 EN LINEAIRE, NBNAPA=3 EN QUADRATIQUE
!
    if (nbnapa .eq. 3) then
        coeff=sqrt(96.d0)
    else
        coeff=sqrt(24.d0)
    endif
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (typnor .eq. 'NRJ') then
!
        nompar(1)='E'
        nompar(2)='NU'
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, ' ', r8bid,&
                    2, nompar, valres, icodre, 1)
        e =valres(1)
        nu=valres(2)
!
        if (nbnapa .eq. 3) then
            coeff=sqrt(96.d0*e/(1-nu))
        else
            coeff=sqrt(24.d0*e/(1-nu))
        endif
!
        r8tmp=sqrt(tvol+tnor+tsau)/coeff
        sigcal=sqrt(norsig)
        zr(ierr-1+1)=r8tmp
        zr(ierr-1+2)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
        zr(ierr-1+3)=sigcal
!
        r8tmp=sqrt(tvol)/coeff
        zr(ierr-1+4)=r8tmp
        zr(ierr-1+5)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
        r8tmp=sqrt(tnor)/coeff
        zr(ierr-1+6)=r8tmp
        zr(ierr-1+7)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
        r8tmp=sqrt(tsau)/coeff
        zr(ierr-1+8)=sqrt(tsau)/coeff
        zr(ierr-1+9)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
    else
!
        r8tmp=(tvol+tnor+tsau)/coeff
        sigcal=sqrt(norsig)
        zr(ierr-1+1)=r8tmp
        zr(ierr-1+2)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
        zr(ierr-1+3)=sigcal
!
        r8tmp=tvol/coeff
        zr(ierr-1+4)=r8tmp
        zr(ierr-1+5)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
        r8tmp=tnor/coeff
        zr(ierr-1+6)=r8tmp
        zr(ierr-1+7)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
        r8tmp=tsau/coeff
        zr(ierr-1+8)=r8tmp
        zr(ierr-1+9)=100.d0*sqrt(r8tmp**2/(r8tmp**2+norsig))
!
    endif
!
    zr(ierr-1+10)=he
!
    call jedema()
!
end subroutine
