subroutine afrela(coefr, coefc, ddl, noeud, ndim,&
                  direct, nbterm, betar, betac, betaf,&
                  typcoe, typval, typlag, epsi, lisrez)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!.======================================================================
    implicit none
!
!       AFRELA -- AFFECTATION D'UNE RELATION A LA .S.D DE TYPE
!                 LISTE_RELA ET DE NOM LISREL
!
!----------------------------------------------------------------------
! BUT : AFFECTATION D'UNE  RELATION ENTRE DDLS A UNE SD LISTE_RELA
!       (SI L'OBJET LISREZ N'EXISTE PAS, IL EST CREE)
!
!----------------------------------------------------------------------
! COEFR(NBTERM) - IN - R   - : TABLEAU DES COEFFICIENTS DE LA RELATION
!                              LES COEFFICIENTS SONT REELS
!----------------------------------------------------------------------
! COEFC(NBTERM) - IN - C   - : TABLEAU DES COEFFICIENTS DE LA RELATION
!                              LES COEFFICIENTS SONT COMPLEXES
!----------------------------------------------------------------------
! DDL(NBTERM)   - IN - K8  - : TABLEAU DES DDL DE LA RELATION
!----------------------------------------------------------------------
! NOEUD(NBTERM) - IN - K8  - : TABLEAU DES NOEUDS DE LA RELATION
!-----------------------------------------------------------------------
! NDIM(NBTERM)  - IN - I   - : DIMENSION DU PROBLEME (0, 2 OU 3)
!                              SI = 0 PAS DE CHANGEMENT DE REPERE
!                              LA RELATION EST DONNEE DANS LA BASE
!                              GLOBALE
!----------------------------------------------------------------------
! DIRECT(3,NBTERM)- IN - R - : TABLEAU DES VECTEURS RELATIFS A CHAQUE
!                              TERME DEFINISSANT LA DIRECTION DE LA
!                              COMPOSANTE QUE L'ON VEUT CONTRAINDRE
!----------------------------------------------------------------------
! NBTERM        - IN - I   - : NOMBRE DE TERMES DE LA RELATION
!----------------------------------------------------------------------
! BETAR         - IN - R   - : VALEUR REELLE DU SECOND MEMBRE
!----------------------------------------------------------------------
! BETAC         - IN - C   - : VALEUR COMPLEXE DU SECOND MEMBRE
!----------------------------------------------------------------------
! BETAF         - IN - K19 - : VALEUR FONCTION DU SECOND MEMBRE
!----------------------------------------------------------------------
! TYPCOE        - IN - K4  - : TYPE DES COEFFICIENTS DE LA RELATION :
!                              = 'REEL' OU 'COMP'
!----------------------------------------------------------------------
! TYPVAL        - IN - K4  - : TYPE DU SECOND MEMBRE
!                              = 'REEL' OU 'COMP' OU 'FONC'
!----------------------------------------------------------------------
! TYPLAG        - IN - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                              ASSOCIES A LA RELATION :
!                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
!                              SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
!----------------------------------------------------------------------
! EPSI       - IN - R  - : VALEUR EN DECA DE LAQUELLE LES COEFFICIENTS
!                          SONT SUPPOSES NULS
!----------------------------------------------------------------------
! LISREZ        - IN - K19 - : NOM DE LA SD LISTE_RELA
!               - JXVAR    -
!----------------------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
!
! -----  ARGUMENTS
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/crelrl.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: coefr(nbterm), dble, dimag, epsi
    real(kind=8) :: direct(3, nbterm), betar2
!
    integer :: ndim(nbterm)
!
    complex(kind=8) :: betac, betac2, coefc(nbterm)
!
    character(len=2) :: typlag
    character(len=4) :: typcoe, typval, typco2
    character(len=*) :: betaf
    character(len=8) :: ddl(nbterm), noeud(nbterm)
    character(len=*) :: lisrez
! ------ VARIABLES LOCALES
!
    integer :: imult
    character(len=1) :: k1bid
    character(len=8) :: ddltra(3), ddlrot(3)
    character(len=19) :: lisrel
!.========================= DEBUT DU CODE EXECUTABLE ==================
!  DEB
!-----------------------------------------------------------------------
    integer :: i, idbeta, idcoef, iddl, idim, idlagr, idnbre
    integer :: idnoeu, idpoin, idsurc, idterm, ifm, ipoint, iret
    integer :: irot, k, kk, lonuti, lveclr, mdim, nbrel0
    integer :: nbrela, nbrmax, nbterm, nbterr, niv
    real(kind=8) :: betar, rcoef
!-----------------------------------------------------------------------
    call jemarq()
    lisrel = lisrez
    typco2 = typcoe
    if (typcoe .eq. 'FONC') typco2 = 'REEL'
    betar2=betar
    betac2=betac
!
!
!     -- IMPRESSION DE LA RELATION SI INFO:2:
!     ---------------------------------------
    call infniv(ifm, niv)
    if (niv .eq. 2) then
        write (ifm,*) ' '
        write (ifm,*) '_RELA IMPRESSION D''UNE RELATION LINEAIRE ENTRE '&
     &    ,nbterm,' DDLS. (AVANT NORMALISATION DE LA RELATION)'
        do 10,k = 1,nbterm
        if (ndim(k) .eq. 0) then
            if (typco2 .eq. 'REEL') then
                write (ifm,1001) coefr(k),noeud(k),ddl(k)
            else if (typco2.eq.'COMP') then
                write (ifm,1003) dble(coefc(k)),dimag(coefc(k)),&
                    noeud(k), ddl(k)
            endif
        else
            if (typco2 .eq. 'REEL') then
                write (ifm,1002) coefr(k),noeud(k),ddl(k),&
                    (direct(kk,k),kk=1,ndim(k))
            else if (typco2.eq.'COMP') then
                write (ifm,1004) dble(coefc(k)),dimag(coefc(k)),&
                    noeud(k), ddl(k), (direct(kk,k),kk=1,ndim(k))
            endif
        endif
10      continue
        if (typval .eq. 'REEL') then
            write (ifm,*) '_RELA = ',betar2
        else if (typval.eq.'COMP') then
            write (ifm,*) '_RELA = ',betac2
        else if (typval.eq.'FONC') then
            write (ifm,*) '_RELA = ',betaf
        endif
    endif
!
!
!     1.1 : CALCUL DU COEF DE NORMALISATION RCOEF :
!     ----------------------------------------------
    if (typco2 .eq. 'REEL') then
        rcoef = 0.d0
        do 20,k = 1,nbterm
        rcoef = max(rcoef,abs(coefr(k)))
20      continue
        if (rcoef .eq. 0.d0) call u2mess('F', 'MODELISA_97')
!
    else if (typco2.eq.'COMP') then
        rcoef = 0.d0
        do 30,k = 1,nbterm
        rcoef = max(rcoef,abs(coefc(k)))
30      continue
        if (rcoef .eq. 0.d0) call u2mess('F', 'MODELISA_97')
    else
        call u2mesk('F', 'MODELISA_98', 1, typco2)
    endif
!
!
    if (typval .eq. 'REEL') then
        betar2= betar2/rcoef
    else if (typval.eq.'COMP') then
        betac2= betac2/rcoef
    else if (typval.eq.'FONC') then
!       -- ON ALARME SI LRCOEF EST TROP DIFFERENT DE 1.
        if ((rcoef.gt.1.d3) .or. (rcoef.lt.1.d-3)) call u2mess('A', 'MODELISA_99')
!       -- ON NE PEUT PAS "DIVISER" LE SECOND MEMBRE (FONCTION) !
        rcoef=1.d0
    endif
!
!
    ddltra(1) = 'DX'
    ddltra(2) = 'DY'
    ddltra(3) = 'DZ'
!
    ddlrot(1) = 'DRX'
    ddlrot(2) = 'DRY'
    ddlrot(3) = 'DRZ'
!
    irot = 0
!
! --- SI L'OBJET LISREL N'EXISTE PAS, ON LE CREE :
!     ------------------------------------------
    call jeexin(lisrel//'.RLCO', iret)
    if (iret .eq. 0) call crelrl(typco2, typval, 'V', lisrel)
!
! --- NOMBRE DE RELATIONS DE LA LISTE_DE_RELATIONS :
!     --------------------------------------------
    call jeveuo(lisrel//'.RLNR', 'E', idnbre)
    nbrel0 = zi(idnbre)
    nbrela = nbrel0 + 1
!
! --- LONGUEUR TOTALE DES VECTEURS RELATIFS A TOUS LES TERMES
! --- DES RELATIONS :
!     -------------
    call jelira(lisrel//'.RLCO', 'LONMAX', lveclr, k1bid)
!
! --- LONGUEUR EFFECTIVEMENT UTILISEE :
!     -------------------------------
    call jeveuo(lisrel//'.RLPO', 'E', idpoin)
    if (nbrel0 .eq. 0) then
        lonuti = 0
    else
        lonuti = zi(idpoin+nbrel0-1)
    endif
!
! --- SI LA RELATION N'EST PAS DONNEE DANS LE REPERE  GLOBAL, ON
! --- REAJUSTE SON NOMBRE DE TERMES, D'AUTRE_PART ON NE TIENT PAS
! --- COMPTE DES TERMES A COEFFICIENT NUL :
!     -----------------------------------
    nbterr = 0
    do 40 i = 1, nbterm
        if (typco2 .eq. 'COMP') then
            if (abs(coefc(i)) .gt. epsi) then
                if (ndim(i) .eq. 0) then
                    nbterr = nbterr + 1
                else
                    nbterr = nbterr + ndim(i)
                endif
            endif
        else
            if (abs(coefr(i)) .gt. epsi) then
                if (ndim(i) .eq. 0) then
                    nbterr = nbterr + 1
                else
                    nbterr = nbterr + ndim(i)
                endif
            endif
        endif
40  end do
!
! --- ON RECREE LES OBJETS RELATIFS AUX TERMES DES RELATIONS EN
! --- AUGMENTANT LA TAILLE DE CES OBJETS SI LEUR LONGUEUR S'AVERE
! --- INSUFFISANTE :
!     ------------
    if (lonuti+nbterr .ge. lveclr) then
        imult = (lonuti+nbterr)/lveclr + 1
        call juveca(lisrel//'.RLCO', imult*lveclr)
        call juveca(lisrel//'.RLDD', imult*lveclr)
        call juveca(lisrel//'.RLNO', imult*lveclr)
    endif
!
! --- NOMBRE MAX DE RELATIONS INITIALEMENT PREVU :
!     ------------------------------------------
    call jelira(lisrel//'.RLNT', 'LONMAX', nbrmax, k1bid)
!
! --- ON RECREE LES OBJETS DIMENSIONNES AU NOMBRE DE RELATIONS
! --- EN AUGMENTANT LA TAILLE DE CES OBJETS  SI LEUR LONGUEUR
! --- S'AVERE INSUFFISANTE :
!     --------------------
    if (nbrela .ge. nbrmax) then
        imult = nbrela/nbrmax + 1
        call juveca(lisrel//'.RLBE', imult*nbrmax)
        call juveca(lisrel//'.RLNT', imult*nbrmax)
        call juveca(lisrel//'.RLPO', imult*nbrmax)
        call juveca(lisrel//'.RLSU', imult*nbrmax)
        call juveca(lisrel//'.RLLA', imult*nbrmax)
    endif
!
! --- AFFECTATION DES COMPOSANTES DE LA RELATION A LA LISTE
! --- DE RELATIONS :
!     ------------
    call jeveuo(lisrel//'.RLNR', 'E', idnbre)
    call jeveuo(lisrel//'.RLCO', 'E', idcoef)
    call jeveuo(lisrel//'.RLDD', 'E', iddl)
    call jeveuo(lisrel//'.RLNO', 'E', idnoeu)
    call jeveuo(lisrel//'.RLBE', 'E', idbeta)
    call jeveuo(lisrel//'.RLNT', 'E', idterm)
    call jeveuo(lisrel//'.RLPO', 'E', idpoin)
    call jeveuo(lisrel//'.RLSU', 'E', idsurc)
    call jeveuo(lisrel//'.RLLA', 'E', idlagr)
!
    zi(idnbre) = nbrela
    zk8(idlagr+nbrela-1) (1:2) = typlag
    if (nbrel0 .eq. 0) then
        ipoint = 0
    else
        ipoint = zi(idpoin+nbrel0-1)
    endif
!
    k = 0
!
! --- CAS DES COEFFICIENTS COMPLEXES :
!     ------------------------------
    if (typco2 .eq. 'COMP') then
        do 60 i = 1, nbterm
! ---   ON NE TIENT COMPTE QUE DES COEFFICIENTS NON-NULS
            if (abs(coefc(i)) .gt. epsi) then
                if (ndim(i) .eq. 0) then
                    k = k + 1
                    zc(idcoef+ipoint+k-1) = coefc(i)/rcoef
                    zk8(iddl+ipoint+k-1) = ddl(i)
                    zk8(idnoeu+ipoint+k-1) = noeud(i)
                else
!
! --- LA NOUVELLE RELATION ECRITE DANS LE REPERE GLOBAL EST
! --- DETERMINEE AVEC LA REGLE DE CORRESPONDANCE  SUIVANTE :
! ---  DEPL --> DIRECT(1)*U  + DIRECT(2)*V  + DIRECT(3)*W
! ---  ROTA --> DIRECT(1)*RX + DIRECT(2)*RY + DIRECT(3)*RZ
!      ---------------------------------------------------
                    if (ddl(i) .eq. 'DEPL') then
                        irot = 0
                    else if (ddl(i).eq.'ROTA') then
                        irot = 1
                    else
                        call assert(.false.)
                    endif
!
                    mdim = ndim(i)
                    do 50 idim = 1, mdim
                        k = k + 1
                        zc(idcoef+ipoint+k-1) = coefc(i)/rcoef*direct( idim,i)
                        zk8(idnoeu+ipoint+k-1) = noeud(i)
                        if (irot .eq. 0) then
                            zk8(iddl+ipoint+k-1) = ddltra(idim)
                        else if (irot.eq.1) then
                            zk8(iddl+ipoint+k-1) = ddlrot(idim)
                        endif
50                  continue
                endif
            endif
60      continue
!
! --- CAS DES COEFFICIENTS REELS :
!     --------------------------
    else
        do 80 i = 1, nbterm
! ---   ON NE TIENT COMPTE QUE DES COEFFICIENTS NON-NULS
            if (abs(coefr(i)) .gt. epsi) then
                if (ndim(i) .eq. 0) then
                    k = k + 1
                    zr(idcoef+ipoint+k-1) = coefr(i)/rcoef
                    zk8(iddl+ipoint+k-1) = ddl(i)
                    zk8(idnoeu+ipoint+k-1) = noeud(i)
                else
!
! --- LA NOUVELLE RELATION ECRITE DANS LE REPERE GLOBAL EST
! --- DETERMINEE AVEC LA REGLE DE CORRESPONDANCE  SUIVANTE :
! ---  DEPL --> DIRECT(1)*U  + DIRECT(2)*V  + DIRECT(3)*W
! ---  ROTA --> DIRECT(1)*RX + DIRECT(2)*RY + DIRECT(3)*RZ
!      ---------------------------------------------------
                    if (ddl(i) .eq. 'DEPL') then
                        irot = 0
                    else if (ddl(i).eq.'ROTA') then
                        irot = 1
                    else
                        call assert(.false.)
                    endif
!
                    mdim = ndim(i)
                    do 70 idim = 1, mdim
                        k = k + 1
                        zr(idcoef+ipoint+k-1) = coefr(i)/rcoef*direct( idim,i)
                        zk8(idnoeu+ipoint+k-1) = noeud(i)
                        if (irot .eq. 0) then
                            zk8(iddl+ipoint+k-1) = ddltra(idim)
                        else if (irot.eq.1) then
                            zk8(iddl+ipoint+k-1) = ddlrot(idim)
                        endif
70                  continue
                endif
            endif
80      continue
    endif
!
    if (typval .eq. 'REEL') then
        zr(idbeta+nbrela-1) = betar2
    else if (typval.eq.'COMP') then
        zc(idbeta+nbrela-1) = betac2
    else if (typval.eq.'FONC') then
        zk24(idbeta+nbrela-1) = betaf
    endif
!
    zi(idterm+nbrela-1) = nbterr
    if (nbrel0 .eq. 0) then
        zi(idpoin) = nbterr
    else
        zi(idpoin+nbrela-1) = zi(idpoin+nbrel0-1) + nbterr
    endif
!
    call jedema()
!
    1001 format (' _RELA ',e14.7,a10,a10)
    1003 format (' _RELA ',e14.7,1x,e14.7,a10,a10)
    1002 format (' _RELA ',e14.7,a10,a10,3x,3 (1x,e14.7))
    1004 format (' _RELA ',e14.7,1x,e14.7,a10,a10,3x,3 (1x,e14.7))
!
end subroutine
