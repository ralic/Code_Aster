subroutine fetggt(nbsd, matas, vsdf, vddl, lrigid,&
                  nbi, nomggt, dimgi, nomgi, stogi,&
                  lstogi, mamoy, infofe, irex, ifm,&
                  sdfeti, nbproc, rang, itps)
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
!    - FONCTION REALISEE:  CALCUL DES MATRICES GI ET (GI)T * GI
!
!      IN  MATAS: K19  : NOM DE LA MATRICE DE RIGIDITE GLOBALE
!      IN   VSDF: VIN  : VECTEUR MATR_ASSE.FETF INDIQUANT SI
!                         SD FLOTTANT
!      IN   NBSD: IN   : NOMBRE DE SOUS-DOMAINES
!      IN   VDDL: VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!     OUT LRIGID: LO  : LOGICAL INDIQUANT LA PRESENCE D'AU MOINS UN
!         SOUS-DOMAINES FLOTTANT
!      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
!      IN NOMGGT: CH24: NOMS DE OBJ. JEVEUX CONTENANT GIT*GI
!     OUT DIMGI:  IN : TAILLE DE GIT*GI
!      IN NOMGI: CH24: NOMS DE OBJ. JEVEUX CONTENANT GI (EVENTUELLEMENT)
!      IN STOGI: CH24: PARAMETRE DE STOCKAGE DE GI
!     OUT LSTOGI: LO : TRUE, GI STOCKE, FALSE, RECALCULE
!      IN MAMOY: IN : CRITERE DE STOCKAGE DE GI SI STOGI='CAL'
!     IN IREX  : IN    : ADRESSE DU VECTEUR AUXILAIRE EVITANT DES APPELS
!                        JEVEUX.
!     IN RANG  : IN  : RANG DU PROCESSEUR
!     IN SDFETI: CH19: SD DECRIVANT LE PARTIONNEMENT FETI
!     IN NBPROC: IN  : NOMBRE DE PROCESSEURS
!     IN ITPS  : IN  : NUMERO DU PAS DE TEMPS
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/fetmpi.h'
    include 'asterfort/fetrex.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    integer :: nbsd, vsdf(nbsd), vddl(nbsd), nbi, dimgi, mamoy, irex, ifm, rang
    integer :: nbproc, itps
    character(len=19) :: matas, sdfeti
    character(len=24) :: nomggt, nomgi, stogi, infofe
    logical :: lrigid, lstogi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, nbmc, ngi, ngitgi, imc, ibid, ifetr, nbddl, opt, idecaj
    integer :: imc1, jmc, gii, gij, idecao, i, j, jgitgi, jdd, idd1, nbddlj
    integer :: nbmcj, icompt, ilimp1, ifetrj, jgi, idecai, ilimpi, nivmpi, iach1
    integer :: iach2, iaux1, iaux11, nbpro1
    real(kind=8) :: raux, rbid
    character(len=8) :: nomsd, nomsdj
    character(len=24) :: nomsdr, sdfetg, nom1, nom2
    logical :: lpara
!
! CORPS DU PROGRAMME
    call jemarq()
!
! INITS
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
    sdfetg=sdfeti//'.FETG'
    if (infofe(10:10) .eq. 'T') then
        nivmpi=2
    else
        nivmpi=1
    endif
    nomsdr=matas//'.FETR'
    lrigid=.false.
    lstogi=.false.
    dimgi=0
    jgitgi=-1
! ADRESSE JEVEUX OBJET FETI & MPI
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
    call jeveuo('&FETI.LISTE.SD.MPIB', 'L', ilimp1)
!
! BOUCLE SUR LES SOUS-DOMAINES POUR CALCULER L'ORDRE DE DE GIT*GI
! ET LA NECESSITE DU STOCKAGE DE GI OU NON
    do 10 idd = 1, nbsd
        dimgi=dimgi+vsdf(idd)
10  end do
    if (dimgi .eq. 0) then
! MONITORING
        if ((infofe(1:1).eq.'T') .and. (rang.eq.0)) write(ifm, *)'<FETI/FETGGT', rang,&
                                                    '> PAS DE MODE DE CORPS RIGIDE'
        goto 999
    else
        lrigid=.true.
        raux=dimgi*nbi
! DETERMINATION DU STOCKAGE DE GI OU PAS
        if (stogi(1:3) .eq. 'OUI') then
            lstogi=.true.
        else if (stogi(1:3).eq.'NON') then
            lstogi=.false.
        else if (stogi(1:3).eq.'CAL') then
            if (raux .lt. mamoy) then
                lstogi=.true.
            else
                lstogi=.false.
            endif
        else
            call u2mess('F', 'ALGORITH3_64')
        endif
! EN PARALLELE
! ATTENTION, POUR NE PAS AVOIR A TRANSFERER LES MATAS.FETH + COLAUI
! AU PROCESSUS MAITRE OU SUPPORTER LES ENVOIS DE MSG CONTINGENT A
! CHAQUE PROJECTION, IL FAUT CONSTRUIRE GI ET GITGI UNE FOIS POUR TOUTE
! LORS DE CE PREMIER PASSAGE DANS FETGGT
! IL N'EST PAS CHOQUANT QUE CETTE FACILITE D'ECONOMIE DE STOCKAGE NE
! RESERVER QU'AU SEQUENTIEL. EN PARALLELE ON PEUT DISTRIBUER AUTREMENT
! LES PROCESSUS POUR LIMITER DE CHARGER LE PROCESSUS MAITRE EN MEMOIRE
        if ((lpara) .and. (.not.lstogi)) call u2mess('F', 'ALGORITH3_65')
!
! MONITORING
        if ((infofe(1:1).eq.'T') .and. (itps.eq.1)) then
            write(ifm,*)'<FETI/FETGGT', rang,&
     &      '> NBRE TOTAL DE CORPS RIGIDES',dimgi
            if (lstogi) write(ifm, *)'              STOCKAGE GI', lstogi, raux, mamoy
        endif
    endif
!
! GI ET GIT*GI NE SONT CALCULEES QU'AU PREMIER PAS DE TEMPS
    if (itps .gt. 1) goto 999
!
! VECTEURS AUXILIAIRES CONTENANT GI ET GIT*GI (STOCKAGE PAR COLONNE
! AVEC SEULEMENT LA PARTIE TRIANGULAIRE INFERIEURE POUR GIT*GI)
! EN PARALLELE, SI RANG NON NUL, ON DETRUIT GI EN FIN DE ROUTINE.
! ON EN A JUSTE BESOIN POUR LE MPI_GATHER
    if (rang .eq. 0) then
        ngitgi=dimgi*(dimgi+1)/2
        call wkvect(nomggt, 'V V R', ngitgi, jgitgi)
    endif
    if (lstogi) then
        ngi=nbi*dimgi
        call wkvect(nomgi, 'V V R', ngi, jgi)
    else
        call wkvect('&&FETI.GGT.V1', 'V V R', nbi, gii)
        call wkvect('&&FETI.GGT.V2', 'V V R', nbi, gij)
    endif
!
    opt=1
    if (lstogi) then
! --------------------------------------------------------------------
! CONSTITUTION DE (GI)T*GI EN CONSTRUISANT GI
! --------------------------------------------------------------------
! DECALAGE DU VECTEUR OUTPUT DE FETREX (GI)
        idecao=jgi
!
! PREPARATION DU TERRAIN POUR LE PARALLELISME SI NECESSAIRE
        if (lpara) then
            nom1='&&RECVCOUNT_FETGGT'
            call wkvect(nom1, 'V V S', nbproc, iach1)
            nom2='&&DISPLS_FETGGT'
            call wkvect(nom2, 'V V S', nbproc, iach2)
            do 14 i = 1, nbproc
                zi4(iach1+i-1)=0
                zi4(iach2+i-1)=0
14          continue
            nbpro1=nbproc-1
            do 16 idd = 1, nbsd
                nbmc=vsdf(idd)
                if (nbmc .ne. 0) then
                    nbmc=nbmc*nbi
                    iaux1=zi(ilimp1+idd-1)
                    zi4(iach1+iaux1)=zi4(iach1+iaux1)+nbmc
                    iaux11=iaux1+1
                    do 15 i = iaux11, nbpro1
                        zi4(iach2+i)=zi4(iach2+i)+nbmc
15                  continue
                endif
16          continue
!
        endif
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        do 30 idd = 1, nbsd
! LE SOUS-DOMAINE IDD EST IL CONCERNE PAR LE PROCESSUS ACTUEL ?
            if (zi(ilimpi+idd) .eq. 1) then
                nbddl=vddl(idd)
                nbmc=vsdf(idd)
                if (nbmc .ne. 0) then
                    call jenuno(jexnum(sdfetg, idd), nomsd)
                    call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
                    do 20 imc = 1, nbmc
                        call fetrex(opt, idd, nbddl, zr(ifetr+(imc-1)* nbddl), nbi,&
                                    zr(idecao), irex)
                        idecao=idecao+nbi
20                  continue
                    call jelibe(jexnom(nomsdr, nomsd))
                endif
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
            endif
30      continue
!
! COLLECTE SELECTIVE DE GI POUR LE PROCESSUS MAITRE
        if (lpara) then
            ibid=zi4(iach1+rang)
            call fetmpi(8, ibid, ifm, nivmpi, rang,&
                        nbproc, nomgi, nom1, nom2, rbid)
        endif
! MONITORING
        if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETGGT', rang, '> CONSTRUCTION GI'
        if ((infofe(4:4).eq.'T') .and. (rang.eq.0)) then
            idecao=jgi
            if (dimgi .le. 5) then
                do 32 j = 1, dimgi
                    do 31 i = 1, nbi
                        write(ifm,*)'G(I,J)',i,j,zr(idecao)
                        idecao=idecao+1
31                  continue
32              continue
                raux=ddot(dimgi*nbi,zr(jgi),1,zr(jgi),1)
                write(ifm,*)'NORME DE G ',abs(raux)
            endif
        endif
!
! DESTRUCTION DES OBJETS AUXILIAIRES ENCOMBRANTS EN PARALLELISME
        if (rang .ne. 0) call jedetr(nomgi)
        if (lpara) then
            call jedetr(nom1)
            call jedetr(nom2)
        endif
!
! IL ME SEMBLE QU'IL N'Y A PAS DE ROUTINE BLAS EFFECTUANT GT*G AVEC
! G MATRICE RECTANGLE ET LE RESULTAT STOCKE DANS UNE MATRICE TRIANG
! GULAIRE. EN PARALLELE, SEUL LE PROCESSEUR ZERO CONSTRUIT GIT*G
        if (rang .eq. 0) then
            idecao=jgitgi
            do 43 j = 1, dimgi
                idecaj=(j-1)*nbi+jgi
                do 42 i = j, dimgi
                    idecai=(i-1)*nbi+jgi
                    zr(idecao)=ddot(nbi,zr(idecai),1,zr(idecaj),1)
                    idecao=idecao+1
42              continue
43          continue
        endif
    else
! --------------------------------------------------------------------
! CONSTITUTION DE (GI)T*GI SANS CONSTRUIRE GI SEULEMENT EN SEQUENTIEL
! --------------------------------------------------------------------
! ----  BOUCLE SUR LES SOUS-DOMAINES
!
! NOMBRE DE SOUS-DOMAINES FLOTTANTS
        icompt=-1
        do 100 idd = 1, nbsd
!
            nbddl=vddl(idd)
            nbmc=vsdf(idd)
!
            if (nbmc .ne. 0) then
                call jenuno(jexnum(sdfetg, idd), nomsd)
                call jeveuo(jexnom(nomsdr, nomsd), 'L', ifetr)
                idd1=idd+1
                do 90 imc = 1, nbmc
                    call fetrex(opt, idd, nbddl, zr(ifetr+(imc-1)*nbddl), nbi,&
                                zr(gii), irex)
                    icompt=icompt+1
                    zr(jgitgi+icompt)=ddot(nbi,zr(gii),1,zr(gii),1)
                    imc1=imc+1
                    do 60 jmc = imc1, nbmc
                        icompt=icompt+1
                        call fetrex(opt, idd, nbddl, zr(ifetr+(jmc-1)* nbddl), nbi,&
                                    zr(gij), irex)
                        zr(jgitgi+icompt)=ddot(nbi,zr(gii),1,zr(gij),&
                        1)
60                  continue
!
! ----  BOUCLE SUR LES SOUS-DOMAINES JDD > IDD
                    do 80 jdd = idd1, nbsd
                        nbddlj=vddl(jdd)
                        nbmcj=vsdf(jdd)
                        if (nbmcj .ne. 0) then
! SOUS-DOMAINE FLOTTANT
! COMPOSANTES DES MODES DE CORPS RIGIDES
                            call jenuno(jexnum(sdfetg, jdd), nomsdj)
                            call jeveuo(jexnom(nomsdr, nomsdj), 'L', ifetrj)
! ----  BOUCLE SUR LES MODES DE CORPS RIGIDES DE JDD
                            do 70 jmc = 1, nbmcj
                                icompt=icompt+1
                                call fetrex(opt, jdd, nbddlj, zr(ifetrj+( jmc-1)*nbddlj), nbi,&
                                            zr(gij), irex)
                                zr(jgitgi+icompt)=ddot(nbi,zr(gii),1,&
                                zr(gij),1)
70                          continue
                            call jelibe(jexnom(nomsdr, nomsdj))
                        endif
80                  continue
90              continue
                call jelibe(jexnom(nomsdr, nomsd))
            endif
100      continue
        call jedetr('&&FETI.GGT.V1')
        call jedetr('&&FETI.GGT.V2')
    endif
!
!
! MONITORING
    if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/FETGGT', rang, '> CONSTRUCTION (GI)T*GI'
    if ((infofe(4:4).eq.'T') .and. (rang.eq.0)) then
        idecao=jgitgi
        if (dimgi .le. 5) then
            do 151 j = 1, dimgi
                do 150 i = j, dimgi
                    write(ifm,*)'GTG(I,J)',i,j,zr(idecao)
                    idecao=idecao+1
150              continue
151          continue
        endif
        raux=ddot(dimgi*dimgi,zr(jgitgi),1,zr(jgitgi),1)
        write(ifm,*)'NORME DE GTG ',abs(raux)
    endif
!
999  continue
!
    call jedema()
end subroutine
