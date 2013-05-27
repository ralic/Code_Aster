subroutine mdallo(nomres, basemo, masgen, riggen, amogen,&
                  nbmode, dt, nbsauv, nbchoc, noecho,&
                  intitu, nbrede, fonred, nbrevi, fonrev,&
                  jdepl, jvite, jacce, jptem, jordr,&
                  jdisc, jfcho, jdcho, jvcho, jadcho,&
                  jredc, jredd, jrevc, jrevd, method,&
                  nbsym, nomsym, typcal, sauve)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: basemo, masgen, riggen, amogen
    character(len=8) :: nomres, intitu(*), kbid, kb
    character(len=8) :: noecho(nbchoc, *), fonred(nbrede, *), fonrev(nbrevi, *)
    character(len=16) :: method
! ----------------------------------------------------------------------
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
! TOLE CRP_21
!
!     ALLOCATION DES VECTEURS DE SORTIE POUR UN CALCUL TRANSITOIRE
!     SUR BASE GENERALISEE (SD_DYNA_GENE)
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM DU RESULTAT
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
! IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
! IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DT     : PAS DE TEMPS
! IN  : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! IN  : INTITU : TABLEAU DES NOMS DES LIAISONS
! IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
! IN  : FONRED : TABLEAU DES FONCTIONS DE RED
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : METHOD : ALGORITHME UTILISE (DEVOGE, EULER, ...)
!                DANS LE CAS ITMI, UN OBJET EST DIFFERENT
! IN  : TYPCAL : VAUT 'HARM' OU 'TRAN'
! IN  : SAUVE :  VAUT 'GLOB' OU 'VOLA'
! ----------------------------------------------------------------------
    integer :: nbsauv, nbstoc, j1refe, nbsym, inom
    character(len=8) :: numgen, blanc
    character(len=5) :: attrib
    character(len=4) :: typcal, nomsym(3), sauve
    character(len=3) :: typsau
    character(len=12) :: bl11pt
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ic, iret, jacce, jadcho, jdcho, jdepl, jchmp
    integer :: jdesc, jfcho, jdisc, jinti, jncho, jordr, jptem
    integer :: jredc, jredd, jredn, jrevc, jrevd, jrevn
    integer :: jrefe, jsst, jvcho, jvint
    integer :: jvite, nbchoc, nbmode, nbrede, nbrevi, nbsto1
    real(kind=8) :: dt
!-----------------------------------------------------------------------
    call jemarq()
    call assert((sauve(1:4).eq.'GLOB'.or.sauve(1:4).eq.'VOLA'))
    if (sauve(1:4) .eq. 'GLOB') typsau='G V'
    if (sauve(1:4) .eq. 'VOLA') typsau='V V'
    nbstoc = nbmode * nbsauv
!
    jdepl=1
    jvite=1
    jacce=1
    jptem=1
    jordr=1
    jdisc=1
    jrefe=1
    jdesc=1
    jchmp=1
    jfcho = 1
    jdcho = 1
    jvcho = 1
    jadcho= 1
    jredc = 1
    jredd = 1
    jrevc = 1
    jrevd = 1
    blanc = '        '
    bl11pt = '           .'
!
    call jeexin(nomres//'           .REFD', iret)
    if (iret .eq. 0) then
! On recupere la numerotation generalisee
        call jeexin(riggen(1:8)//'           .REFA', iret)
        if (iret .ne. 0) then
            call jeveuo(riggen(1:8)//'           .REFA', 'L', j1refe)
            numgen = zk24(j1refe+1)(1:8)
        else
            numgen = blanc
        endif
        call wkvect(nomres//'           .REFD', typsau//' K24', 5, jrefe)
        zk24(jrefe) = riggen(1:8)
        zk24(jrefe+1) = masgen(1:8)
        zk24(jrefe+2) = amogen(1:8)
        zk24(jrefe+3) = numgen(1:8)
        zk24(jrefe+4) = basemo(1:8)
    endif
!
    call jeexin(nomres//'           .DESC', iret)
    if (iret .eq. 0) then
        call wkvect(nomres//'           .DESC', typsau//' I', 5, jdesc)
!
        zi(jdesc) = 1
!
        if (typcal .eq. 'HARM') then
            zi(jdesc) = 4
!          -- DANS LE CAS 'HARM' ON REMPLIT LA VALEUR A 4
!
!          -- BLINDAGE : VERIFICATION DE NBSYM ET NOMSYM
            if ((nbsym.le.0) .or. (nbsym.ge.4)) then
                call u2mess('F', 'ALGORITH17_29')
            endif
            do 50, inom = 1,nbsym
            if ((nomsym(inom)(1:4).ne.'DEPL') .and. (nomsym(inom)( 1:4).ne.'VITE') .and.&
                (nomsym(inom)(1:4).ne.'ACCE')) then
                call u2mess('F', 'ALGORITH17_29')
            endif
50          continue
        else if (typcal.eq.'TRAN') then
!         -- INITIALISATION DES CHAMPS A ALLOUER DANS LE CAS TRANS.
            nbsym = 3
            nomsym(1) = 'DEPL'
            nomsym(2) = 'VITE'
            nomsym(3) = 'ACCE'
            if (nbchoc .ne. 0) then
                zi(jdesc) = 2
            endif
!         -DANS LE CAS ITMI ET ADAPT (METHODES A PAS VARIABLE),
!          ON MET LA VALEUR 3 QUI SERVIRA DE TEST
!           A LA COMMANDE POST_DYNA_MODA_T
            if (method .eq. 'ITMI' .or. method(1:5) .eq. 'ADAPT' .or. method( 1:5) .eq.&
                'RUNGE') then
                zi(jdesc) = 3
            endif
!         DANS LE CAS TRANSITOIRE, ON REMPLIT TOUJOURS LES TROIS CHAMPS
        endif
!        ---
        zi(jdesc+1) = nbmode
        zi(jdesc+2) = nbchoc
        zi(jdesc+3) = nbrede
        zi(jdesc+4) = nbrevi
    endif
!
    if (typcal .eq. 'TRAN') then
        attrib = typsau//' R'
        nbsym = 3
        nomsym(1) = 'DEPL'
        nomsym(2) = 'VITE'
        nomsym(3) = 'ACCE'
    else
        attrib = typsau//' C'
    endif
!
    if (nbsauv .ne. 0) then
!       BOUCLE SUR LES CHAMPS A SAUVEGARDER (DEPL/VITE/ACCE)
        do 140 inom = 1, nbsym
!
            call jecreo(nomres//bl11pt//nomsym(inom), attrib)
            call jeecra(nomres//bl11pt//nomsym(inom), 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//bl11pt//nomsym(inom), 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//bl11pt//nomsym(inom), 'E', jchmp)
!
!         INITIALISATION DES CHAMPS A ZERO
!
            if (typcal .eq. 'TRAN') then
                do 150, i = 0,nbstoc-1
                zr(jchmp+i) = 0.d0
150              continue
            else
                do 160, i = 0,nbstoc-1
                zc(jchmp+i) = dcmplx(0.d0,0.d0)
160              continue
            endif
            if (nomsym(inom) .eq. 'DEPL') jdepl=jchmp
            if (nomsym(inom) .eq. 'VITE') jvite=jchmp
            if (nomsym(inom) .eq. 'ACCE') jacce=jchmp
140      continue
!
!       OBJETS COMMUNS
        call jecreo(nomres//'           .ORDR', typsau//' I')
        call jeecra(nomres//'           .ORDR', 'LONMAX', nbsauv, kbid)
        call jeecra(nomres//'           .ORDR', 'LONUTI', nbsauv, kbid)
        call jeveut(nomres//'           .ORDR', 'E', jordr)
        call jecreo(nomres//'           .DISC', typsau//' R')
        call jeecra(nomres//'           .DISC', 'LONMAX', nbsauv, kbid)
        call jeecra(nomres//'           .DISC', 'LONUTI', nbsauv, kbid)
        call jeveut(nomres//'           .DISC', 'E', jdisc)
!
        if (typcal .eq. 'TRAN') then
            call jecreo(nomres//'           .PTEM', typsau//' R')
            call jeecra(nomres//'           .PTEM', 'LONMAX', nbsauv, kbid)
            call jeecra(nomres//'           .PTEM', 'LONUTI', nbsauv, kbid)
            call jeveut(nomres//'           .PTEM', 'E', jptem)
            zr(jptem) = dt
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES FORCES DE CHOC ---
    if (nbchoc .ne. 0) then
        nbstoc = 3 * nbchoc * nbsauv
        nbsto1 = nbchoc * nbsauv
        call jeexin(nomres//'           .NCHO', iret)
        if (iret .eq. 0) call wkvect(nomres//'           .NCHO', typsau//' K8', 2*nbchoc, jncho)
        call jeexin(nomres//'           .SST', iret)
        if (iret .eq. 0) call wkvect(nomres//'           .SST', typsau//' K8', 2*nbchoc, jsst)
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .FCHO', typsau//' R')
            call jeecra(nomres//'           .FCHO', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .FCHO', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .FCHO', 'E', jfcho)
            call jecreo(nomres//'           .DLOC', typsau//' R')
            call jeecra(nomres//'           .DLOC', 'LONMAX', 2*nbstoc, kbid)
            call jeecra(nomres//'           .DLOC', 'LONUTI', 2*nbstoc, kbid)
            call jeveut(nomres//'           .DLOC', 'E', jdcho)
            call jecreo(nomres//'           .VCHO', typsau//' R')
            call jeecra(nomres//'           .VCHO', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .VCHO', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .VCHO', 'E', jvcho)
            call jecreo(nomres//'           .ICHO', typsau//' I')
            call jeecra(nomres//'           .ICHO', 'LONMAX', nbsto1, kbid)
            call jeecra(nomres//'           .ICHO', 'LONUTI', nbsto1, kbid)
            call jeveut(nomres//'           .ICHO', 'E', jadcho)
!          --- OBJET POUR LE FLAMBEMENT : VARIABLE INTERNE ---
            call jecreo(nomres//'           .VINT', typsau//' R')
            call jeecra(nomres//'           .VINT', 'LONMAX', nbsto1, kb)
            call jeecra(nomres//'           .VINT', 'LONUTI', nbsto1, kb)
!              INITIALISATION
            call jeveuo(nomres//'           .VINT', 'E', jvint)
            call r8inir(nbsto1, 0.d0, zr(jvint), 1)
        endif
        call jeexin(nomres//'           .INTI', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .INTI', typsau//' K8', nbchoc, jinti)
            do 10 ic = 1, nbchoc
                zk8(jinti+ic-1) = intitu(ic)
                zk8(jncho+ic-1) = noecho(ic,1)
                zk8(jncho+nbchoc+ic-1) = noecho(ic,5)
                zk8(jsst+ic-1) = noecho(ic,2)
                zk8(jsst+nbchoc+ic-1) = noecho(ic,6)
10          continue
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES RELA_EFFO_DEPL ---
    if (nbrede .ne. 0) then
        nbstoc = nbrede * nbsauv
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .REDC', typsau//' I')
            call jeecra(nomres//'           .REDC', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .REDC', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .REDC', 'E', jredc)
            call jecreo(nomres//'           .REDD', typsau//' R')
            call jeecra(nomres//'           .REDD', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .REDD', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .REDD', 'E', jredd)
        endif
        call jeexin(nomres//'           .REDN', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .REDN', typsau//' K24', nbrede, jredn)
            do 20 i = 1, nbrede
                zk24(jredn+i-1) = fonred(i,1)//fonred(i,2)//fonred(i, 3)
20          continue
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES RELA_EFFO_VITE ---
    if (nbrevi .ne. 0) then
        nbstoc = nbrevi * nbsauv
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .REVC', typsau//' I')
            call jeecra(nomres//'           .REVC', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .REVC', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .REVC', 'E', jrevc)
            call jecreo(nomres//'           .REVD', typsau//' R')
            call jeecra(nomres//'           .REVD', 'LONMAX', nbstoc, kbid)
            call jeecra(nomres//'           .REVD', 'LONUTI', nbstoc, kbid)
            call jeveut(nomres//'           .REVD', 'E', jrevd)
        endif
        call jeexin(nomres//'           .REVN', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .REVN', typsau//' K24', nbrevi, jrevn)
            do 30 i = 1, nbrevi
                zk24(jrevn+i-1) = fonrev(i,1)//fonrev(i,2)//fonrev(i, 3)
30          continue
        endif
    endif
!
    call jedema()
end subroutine
