subroutine fetfiv(nbsd, nbi, vd1, vd2, vdo,&
                  matas, vsdf, vddl, infofe, irex,&
                  ifiv, nbproc, rang, k24irz, sdfeti)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DU PRODUIT OPERATEUR_FETI * V
!
!      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
!      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
!      IN    VD1: VR8  : VECTEUR V DE TAILLE NBI
!      IN    VD2: VR8  : VECTEUR AUXILIAIRE DE TAILLE NBI
!      OUT   VDO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
!      IN  MATAS: CH19 : NOM DE LA MATR_ASSE GLOBALE
!      IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI
!                         SD FLOTTANT
!      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!     IN IREX/IFIV: IN: ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN K24IRZ : K24 : NOM DE L'OBJET JEVEUX VDO POUR LE PARALLELISME
!     IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
!----------------------------------------------------------------------
! TOLE CRP_4
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/fetmpi.h'
    include 'asterfort/fetrex.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rltfr8.h'
    include 'blas/daxpy.h'
    integer :: nbsd, nbi, vddl(nbsd), vsdf(nbsd), irex, ifiv, rang, nbproc
    real(kind=8) :: vd1(nbi), vd2(nbi), vdo(nbi)
    character(len=19) :: matas, sdfeti
    character(len=24) :: infofe, k24irz
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, ifetm, nbddl, idd1, jxsol, j, typsym, j1, nivmpi, ilimpi
    integer :: lmat, nbmc, ifetp, nbmc1, jxsol1, iauxj, k, lconl, ifm, ibid
    integer :: lcon1
    real(kind=8) :: rbid
    character(len=8) :: nomsd
    character(len=19) :: matdd
    character(len=24) :: nomsdp, sdfetg, k24b
    logical :: lpara
    integer(kind=4) :: nbi4
!
! ROUTINE AVEC MOINS DE MONITORING, JEVEUX.. CAR APPELLEE SOUVENT
!
! INITS DIVERSES
    nbi4=nbi
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
! ADRESSE JEVEUX OBJET FETI & MPI
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
! INIT. NOM OBJET JEVEUX POUR PRODUIT PAR PSEUDO-INVERSE LOCALE
    nomsdp=matas//'.FETP'
! INIT POUR JENUNO
    sdfetg=sdfeti//'.FETG'
!
! INIT. VECTEUR SOLUTION ET AUX
    do 10 j = 1, nbi
        vdo(j)=0.d0
10  end do
!
! OBJET JEVEUX POINTANT SUR LA LISTE DES MATR_ASSE
    ifetm=zi(ifiv+1)
    ifm=zi(ifiv)
!
! MONITORING
    if (infofe(1:1) .eq. 'T') write(ifm,*)'<FETI/FETFIV', rang,'> CALCUL FI*V'
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    do 40 idd = 1, nbsd
! LE SOUS-DOMAINE IDD EST IL CONCERNE PAR LE PROCESSUS ACTUEL ?
        if (zi(ilimpi+idd) .eq. 1) then
            idd1=idd-1
!
! MATR_ASSE ASSOCIEE AU SOUS-DOMAINE IDD
            matdd=zk24(ifetm+idd1)(1:19)
! DESCRIPTEUR DE LA MATRICE DU SOUS-DOMAINE
            k=ifiv+2+idd1*5
            lmat=zi(k)
! ADRESSE MATDD.CONL SI IL EXISTE
            lconl=zi(k+1)
! NOMBRE DE BLOC DE STOCKAGE DE LA MATRICE KI/ TYPE DE SYMETRIE
            typsym=zi(lmat+4)
! NBRE DE DDL DU SOUS-DOMAINE IDD
            nbddl=vddl(idd)
! VECTEUR AUXILIAIRE DE TAILLE NDDL(SOUS_DOMAINE_IDD)
            jxsol=zi(k+3)
!
! EXTRACTION DU VECTEUR V AU SOUS-DOMAINE IDD: (RIDD)T * V
            call fetrex(2, idd, nbi, vd1, nbddl,&
                        zr(jxsol), irex)
!
! SCALING VIA ALPHA DES COMPOSANTES DU SECOND MEMBRE DUES AUX LAGRANGES
! SYSTEME: K * U= ALPHA * F ---> K * U/ALPHA = F
            if (lconl .ne. 0) then
                lcon1=zi(k+2)
                do 15 j = 1, nbddl
                    j1=j-1
                    zr(jxsol+j1)=zr(lcon1+j1)*zr(jxsol+j1)
15              continue
            endif
! -------------------------------------------------
! ----  SOUS-DOMAINE NON FLOTTANT
! -------------------------------------------------
! NOMBRES DE MODES DE CORPS RIGIDES DU SOUS-DOMAINE IDD
            nbmc=vsdf(idd)
            if (nbmc .eq. 0) then
!
! CALCUL DE (KIDD)- * FIDD PAR MULT_FRONT
                call rltfr8(matdd, nbddl, zr(jxsol), 1, typsym)
!
            else
! -------------------------------------------------
! ----  SOUS-DOMAINE FLOTTANT
! -------------------------------------------------
! CALCUL DE (KI)+FI PAR MULT_FRONT
                call rltfr8(matdd, nbddl, zr(jxsol), 1, typsym)
                call jenuno(jexnum(sdfetg, idd), nomsd)
                call jeveuo(jexnom(nomsdp, nomsd), 'L', ifetp)
!
                nbmc1=nbmc-1
                jxsol1=jxsol-1
                do 25 j = 0, nbmc1
                    iauxj=zi(ifetp+j)
                    zr(jxsol1+iauxj)=0.d0
25              continue
                call jelibe(jexnom(nomsdp, nomsd))
            endif
! SCALING DES COMPOSANTES DE ZR(LXSOL) POUR CONTENIR LA SOL. REELLE U
            if (lconl .ne. 0) then
                do 30 j = 1, nbddl
                    j1=j-1
                    zr(jxsol+j1)=zr(lcon1+j1)*zr(jxsol+j1)
30              continue
            endif
!
! RESTRICTION DU SOUS-DOMAINE IDD SUR L'INTERFACE: (RIDD) * ...
            call fetrex(1, idd, nbddl, zr(jxsol), nbi,&
                        vd2, irex)
! CUMUL DANS LE VECTEUR VDO=SOMME(I=1,NBSD)(RI * ((KI)+ * RIT * V))
            call daxpy(nbi4, 1.d0, vd2, 1, vdo,&
                       1)
! FIN DU IF ILIMPI
        endif
40  end do
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    if (lpara) then
! REDUCTION DU RESULTAT Z=FI*V POUR LE PROCESSUS MAITRE
        call fetmpi(7, nbi, ifm, nivmpi, ibid,&
                    ibid, k24irz, k24b, k24b, rbid)
    endif
end subroutine
