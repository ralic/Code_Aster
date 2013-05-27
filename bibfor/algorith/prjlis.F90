subroutine prjlis(moda, maa, modb, mab, nbnoa,&
                  nbnob, motcle, linta, lintb, intfa,&
                  intfb, fpliao, fplibo, iada, iadb,&
                  numlis, matprj, modgen, ssta, sstb)
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
!***********************************************************************
    implicit none
!  O. NICOLAS     DATE 01/08/04
!-----------------------------------------------------------------------
!  BUT : < CALCUL DE LA MATRICE DE PROJECTION >
!
!  CALCULER LA MATRICE DE PROJECTION D'UNE INTERFACE ESCLAVE SUR UNE
!  MAITRE POUR LA SOUS STRUCTURATION DYNAMIQUE
!
!-----------------------------------------------------------------------
!
! MODA /I/ : NOM DU MODELE MAITRE
! MODB /I/ : NOM DU MODELE ESCLAVE
! MAA /I/ : NOM DU MAILLAGE MAITRE
! MAB /I/ : NOM DU MAILLAGE ESCLAVE
! NBNOA /I/ : NOMBRE DE NOEUDS D'INTERFACE DU MAILLAGE MAITRE
! NBNOB /I/ : NOMBRE DE NOEUDS D'INTERFACE DU MAILLAGE ESCLAVE
! MOTCLE /I/ : MOT CLE RENSEIGNANT LE GROUPE OU LA MAILLE MAITRE
! LINTA /I/ : NOM DE L'INTERFACE AMONT MAITRE
! LINTB /I/ : NOM DE L'INTERFACE AMONT ESCLAVE
! INTFA /I/ : NOM DE L'INTERFACE MAITRE
! INTFB /I/ : NOM DE L'INTERFACE ESCLAVE
! MATPRJ /O/ : NOM DE LA MATRICE D'OBSERVATION
! NUMLIS /I/ : NUMERO INTERFACE COURANTE
! FPLIAO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTA
! FPLIBO /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES SSTB
! IADA   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTA
! IADB   /I/ : VECTEUR DES CARACTERISTIQUES LIAISON SSTB
! MODGEN  /I/ : NOM K8 DU MODELE GENERALISE
! SSTA    /I/ : NOM K8 DE LA SOUS-STRUCTURE MAITRE
! SSTB    /I/ : NOM K8 DE LA SOUS-STRUCTURE ESCLAVE
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/geolis.h'
    include 'asterfort/infniv.h'
    include 'asterfort/isdeco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pj2dco.h'
    include 'asterfort/pj3dco.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!
    character(len=4) :: zcst
    character(len=8) :: k8bid, linta, lintb, moda, modb, maa, mab, intfa, intfb
    character(len=8) :: matprj, nonob, nonoa, nomg, modgen, ssta, sstb
    character(len=16) :: tymocl(2), motcle(2), motfac, corres
    character(len=24) :: inta, intb, fpliao, fplibo, toto, geoma, geomb
    integer :: ibid, ier, nbnoa, nbnob, llinta, llintb, nbmaa, iagmaa, ndim
    integer :: iaconb, iaconu, iacocf, nbnob2, idecal, inoa, inob, nbterm
    integer :: itemcm, itemtm, nnoa, nunoa, nunob, nunoa2, nunoai, nunobi, i, j
    integer :: iinob, llplia, llplib, icompa, icompb, iadoa, iadob, nbec, ierd
    integer :: numlis, iada(3), iadb(3), nbcmpm, ldesca, ldescb, ifm, niv
    parameter      (nbcmpm=10)
    integer :: idecoa(nbcmpm), idecob(nbcmpm)
    real(kind=8) :: rbid, beta, coefa
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!     -- IMPRESSION DE LA RELATION SI INFO:2:
!     ---------------------------------------
    call infniv(ifm, niv)
!
    motfac='LIAISON'
    tymocl(1) = 'MAILLE'
    tymocl(2) = 'GROUP_MA'
!
!--------------LES NOMBRES DES NOEUDS DES INTERFACES
    inta=linta//'.IDC_LINO'
    call jenonu(jexnom(inta(1:13)//'NOMS', intfa), ibid)
    call jelira(jexnum(inta, ibid), 'LONMAX', nbnoa, k8bid)
!
    intb=lintb//'.IDC_LINO'
    call jenonu(jexnom(intb(1:13)//'NOMS', intfb), ibid)
    call jelira(jexnum(intb, ibid), 'LONMAX', nbnob, k8bid)
!
!--------------LES LISTES DES NUMEROS DES NOEUDS DES INTERFACES
    call jenonu(jexnom(linta //'.IDC_NOMS', intfa), ibid)
    call jeveuo(jexnum(linta //'.IDC_LINO', ibid), 'L', llinta)
    call jeveuo(linta//'.IDC_DEFO', 'L', ldesca)
!
    call jenonu(jexnom(lintb //'.IDC_NOMS', intfb), ibid)
    call jeveuo(jexnum(lintb //'.IDC_LINO', ibid), 'L', llintb)
    call jeveuo(lintb//'.IDC_DEFO', 'L', ldescb)
!
!
!--------------LE NOMBRE DES MAILLES DE L'INTERFACE MAITRE
    call reliem(moda, maa, 'NU_MAILLE', motfac, 1,&
                2, motcle, tymocl, '&&PRJLIS.LIMANUA', nbmaa)
!--------------LA LISTE DES NUMEROS DES MAILLES DE L'INTERFACE MAITRE
    call jeveuo('&&PRJLIS.LIMANUA', 'L', iagmaa)
!
!---ON FAIT LA PROJECTION
    ndim = 3
    call dismoi('F', 'Z_CST', maa, 'MAILLAGE', ibid,&
                zcst, ier)
    if (zcst .eq. 'OUI') ndim = 2
!
    corres = '&&PRJLIS.CORRES'
!
!--------------TRANSFORMATION DE LA GEOMETRIE POUR LA PROJECTION
    geoma = '&&PRJLIS.GEOM_TRANSA'
    geomb = '&&PRJLIS.GEOM_TRANSB'
    call geolis(modgen, ssta, sstb, intfa, intfb,&
                geoma, geomb, '&&PRJLIS.LIMANUA', nbmaa)
!
!--------------CALCUL DE CORRES
!
    if (ndim .eq. 2) then
        call pj2dco('PARTIE', moda, modb, nbmaa, zi(iagmaa),&
                    nbnob, zi(ldescb), geoma, geomb, corres,&
                    .false., rbid)
    else if (ndim.eq.3) then
        call pj3dco('PARTIE', moda, modb, nbmaa, zi(iagmaa),&
                    nbnob, zi(ldescb), geoma, geomb, corres,&
                    .false., rbid)
    endif
!
    call jeveuo(corres//'.PJEF_NB', 'L', iaconb)
    call jeveuo(corres//'.PJEF_NU', 'L', iaconu)
    call jeveuo(corres//'.PJEF_CF', 'L', iacocf)
    call jelira(corres//'.PJEF_NB', 'LONMAX', nbnob2, k8bid)
!
!      CALL UTIMSD(6,2,.TRUE.,.TRUE.,'&&PRJLIS.CORRES',1,' ')
!
!
!-------------ON RECUPERE LES COEFFICIENTS DE LA PROJECTION
    toto='TUTU'
    call wkvect(toto, 'V V R', nbnob*nbnoa, itemtm)
!
! Initialisation de la matrice d'observation
    do 445 inob = 1, nbnob
        do 444 inoa = 1, nbnoa
            zr(itemtm+(inob-1)*nbnoa+inoa-1)=0.d0
444      continue
445  end do
!
! Remplissage et impression de la matrice d'observation
    idecal = 0
    beta = 0.0d0
    nbterm=0
    iinob=1
! boucle sur l'ensemble des noeuds du modele maitre
    do 10 inob = 1, nbnob2
! on recupere le nombre de noeuds maitre lie au noeud esclave courant
        nnoa = zi(iaconb+inob-1)
        nbterm=nnoa+1
! si le nbre de noeud maitre lie au noeud esclave courant est > 0
        if (nnoa .gt. 0) then
            nunob=zi(llintb+iinob-1)
            nunobi=zi(ldescb+nunob-1)
            call jenuno(jexnum(mab//'.NOMNOE', nunobi), nonob)
            if (niv .eq. 2) then
                write (ifm,*) ' '
                write (ifm,*) '_RELA IMPRESSION D''UNE RELATION&
     &            LINEAIRE ENTRE '&
     &            ,nbterm,' DDLS. (AVANT NORMALISATION DE LA RELATION)'
                write (ifm,1001) -1.d0,nonob
            endif
! boucle sur le nombre de noeud maitre lie au noeud esclave courant
            do 30,inoa = 1,nnoa
            nunoa = zi(iaconu+idecal-1+inoa)
            coefa = zr(iacocf+idecal-1+inoa)
            call jenuno(jexnum(maa//'.NOMNOE', nunoa), nonoa)
! boucle sur le nombre de noeud maitre present dans l'interface
            do 40 j = 1, nbnoa
! si le noeud maitre courant est present dans la liste des noeuds
! maitres d'interface on stocke la valeur du coefficient
                nunoa2=zi(llinta+j-1)
                nunoai=zi(ldesca+nunoa2-1)
                if (nunoa .eq. nunoai) then
! On stocke la valeur du coefficient dans la matrice d'observation
! le stockage est donc C(Nbre Noeud esclave,Nbre Noeud maitre)
! l'ordre est donc celui de l'interface esclave pour
! les lignes de la matrice et celui de l'interface maitre pour les
! colonnes
                    zr(itemtm+(iinob-1)*nbnoa+j-1)=coefa
                    if (niv .eq. 2) then
                        write (ifm,1001) coefa,nonoa
                    endif
                endif
40          continue
30          continue
            if (niv .eq. 2) then
                write (ifm,*) '_RELA = ',beta
            endif
            idecal = idecal+nnoa
            iinob=iinob+1
        endif
10  end do
!
! ************************************************************
! Recuperation des donnees par composantes
    nomg = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ierd)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
    call jeveuo(jexnum(fpliao, numlis), 'L', llplia)
    call jeveuo(jexnum(fplibo, numlis), 'L', llplib)
!
    call wkvect(matprj, 'G V R', iada(1)*iadb(1), itemcm)
! Initialisation de la matrice d'observation
    do 446 inob = 1, iadb(1)
        do 447 inoa = 1, iada(1)
            zr(itemcm+(inob-1)*iada(1)+inoa-1)=0.d0
447      continue
446  end do
!
    do 110 inob = 1, nbnob
        iadob=zi(llplib+(inob-1)*(1+nbec))
        call isdeco(zi(llplib+(inob-1)*(1+nbec)+1), idecob, nbcmpm)
        icompb=iadob-1
        do 120 i = 1, nbcmpm
            if (idecob(i) .gt. 0) then
                icompb=icompb+1
                do 130 inoa = 1, nbnoa
                    iadoa=zi(llplia+(inoa-1)*(1+nbec))
                    call isdeco(zi(llplia+(inoa-1)*(1+nbec)+1), idecoa, nbcmpm)
                    icompa=iadoa-1
                    do 140 j = 1, nbcmpm
                        if ((idecoa(j).gt.0) .and. (i.eq.j)) then
! On se limite au repere globaux
                            icompa=icompa+i
                            zr(itemcm+(icompb-1)*iada(1)+icompa-1)=&
                            zr(itemtm+(inob-1)*nbnoa+inoa-1)
                        endif
140                  continue
130              continue
            endif
120      continue
110  end do
!
!
! ************************************************************
!
!-------------FORMAT D'IMPRESSION
    1001 format (' _RELA ',e14.7,a10,a10)
!
    call jedetr(toto)
    call jedetr(geoma)
    call jedetr(geomb)
    call jedetr(corres)
    call jedetr('&&PRJLIS')
!
    call jedema()
end subroutine
