subroutine nmext3(noma, champ, nomcha, nomchs, nbcmp,&
                  nbma, nbpi, nbspi, extrga, extrch,&
                  extrcp, listma, listpi, listsp, listcp,&
                  chgaus, chelga)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nmextj.h'
    include 'asterfort/sdmpic.h'
    integer :: nbcmp, nbma, nbpi, nbspi
    character(len=8) :: noma
    character(len=24) :: nomcha, nomchs
    character(len=8) :: extrcp, extrch, extrga
    character(len=24) :: listma, listpi, listsp, listcp
    character(len=19) :: chgaus, chelga
    character(len=19) :: champ
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS - CAS DES CHAMPS AUX POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  CHAMP  : CHAMP EXTRACTE
! IN  NOMCHA : NOM DU CHAMP
! IN  NOMCHS : NOM DU CHAMP SIMPLE
! IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
! IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! IN  LISTCP : LISTE DES COMPOSANTES
! IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
! IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
! IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
! IN  CHGAUS : VECTEUR DE TRAVAIL CHAMPS AUX POINTS DE GAUSS
! IN  CHAMP  : CHAMP A EXTRAIRE
!
! ----------------------------------------------------------------------
!
    integer :: nparx
    parameter    (nparx=20)
    real(kind=8) :: valres(nparx)
!
    integer :: jgaus, jelga
    integer :: jma, jpi, jspi
    integer :: ima, ipi, ispi, imar, ipir, ispir, nummai
    integer :: num, snum, iret
    integer :: nmapt, nmaspt, npi, nspi
    character(len=8) :: nommai
    integer :: ivalcp, nvalcp
    real(kind=8) :: valr, val2r
    integer :: jcesd, jcesl, jcesv, jcesc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CONVERSION EN CHAM_ELEM_S
!
    call jeexin(nomchs, iret)
    if (iret .eq. 0) then
        call sdmpic('CHAM_ELEM', champ)
        call celces(champ, 'V', nomchs)
    endif
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    call jeveuo(chgaus, 'E', jgaus)
    call jeveuo(chelga, 'E', jelga)
    call jeveuo(nomchs(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(nomchs(1:19)//'.CESL', 'L', jcesl)
    call jeveuo(nomchs(1:19)//'.CESV', 'L', jcesv)
    call jeveuo(nomchs(1:19)//'.CESC', 'L', jcesc)
    call assert(nbcmp.le.nparx)
!
! --- ACCES LISTE DES MAILLES/POINTS/SOUS_POINTS
!
    call jeveuo(listma, 'L', jma)
    call jeveuo(listpi, 'L', jpi)
    call jeveuo(listsp, 'L', jspi)
!
! --- BOUCLE SUR LES MAILLES
!
    do 30 ima = 1, nbma
!
! ----- MAILLE COURANTE
!
        nummai = zi(jma-1+ima)
        call jenuno(jexnum(noma(1:8)//'.NOMMAI', nummai), nommai)
!
! ----- NOMBRE EFFECTIF DE POINTS/SOUS-POINTS SUR LA MAILLE
!
        nmapt = zi(jcesd+5+4*(nummai-1)+1-1)
        nmaspt = zi(jcesd+5+4*(nummai-1)+2-1)
!
! ----- PLAFONNEMENT
!
        npi = nbpi
        nspi = nbspi
        if (npi .gt. nmapt) npi = nmapt
        if (nspi .gt. nmaspt) nspi = nmaspt
!
! ----- CALCUL DES COMPOSANTES SUR LES POINTS/SOUS_POINTS
!
        do 45 ipi = 1, npi
            do 46 ispi = 1, nspi
!
! --------- NUMERO DES POINTS/SOUS-POINTS
!
                num = zi(jpi-1+ipi )
                snum = zi(jspi-1+ispi)
!
! --------- EXTRACTION DES VALEURS AUX POINTS DE GAUSS
!
                call nmextj(nomcha, nbcmp, listcp, extrcp, num,&
                            snum, nvalcp, nummai, jcesd, jcesv,&
                            jcesl, jcesc, valres)
!
! --------- INDICE D'ACCES
!
                if (extrga .eq. 'VALE') then
                    ipir = ipi
                    ispir = ispi
                else
                    ipir = 1
                    ispir = 1
                endif
!
! --------- CALCUL DES VALEURS
!
                do 47 ivalcp = 1, nvalcp
                    valr = valres(ivalcp)
                    val2r = zr( jgaus+nbcmp*(ivalcp-1) +nbpi*(ipir-1) +nbspi*(ispir-1) )
                    if (extrga .eq. 'VALE') then
                        zr(jgaus+nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1)&
                        +(ispir-1)) = valr
                    else if (extrga.eq.'MAX') then
                        zr(jgaus+nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1)&
                        +(ispir-1)) = max(valr,val2r)
                    else if (extrga.eq.'MIN') then
                        zr(jgaus+nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1)&
                        +(ispir-1)) = min(valr,val2r)
                    else if (extrga.eq.'MOY') then
                        zr(jgaus+nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1)&
                        +(ispir-1)) = valr+val2r
                    else
                        call assert(.false.)
                    endif
47              continue
46          continue
45      continue
!
! ----- AFFECTATION DES VALEURS AUX MAILLES
!
        do 75 ipi = 1, npi
            do 76 ispi = 1, nspi
                if (extrga .eq. 'VALE') then
                    ipir = ipi
                    ispir = ispi
                else
                    ipir = 1
                    ispir = 1
                endif
!
                do 77 ivalcp = 1, nvalcp
                    if (extrch .eq. 'VALE') then
                        imar = ima
                    else
                        imar = 1
                    endif
                    valr = zr( jgaus+nbpi*nbspi*(ivalcp-1) +nbspi*( ipir-1) +(ispir-1) )
                    val2r = zr(&
                            jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1&
                            &) +(ispir-1)&
                            )
                    if (extrch .eq. 'VALE') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        valr
                    else if (extrch.eq.'MAX') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        max(valr,val2r)
                    else if (extrch.eq.'MIN') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        min(valr,val2r)
                    else if (extrch.eq.'MAXI_ABS') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        max(abs(val2r),abs(valr))
                    else if (extrch.eq.'MINI_ABS') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        min(abs(val2r),abs(valr))
                    else if (extrch.eq.'MOY') then
                        zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*&
                        nbspi*(ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) =&
                        valr+val2r
                    else
                        call assert(.false.)
                    endif
77              continue
76          continue
75      continue
30  end do
!
! --- CALCUL DE LA MOYENNE
!
    if (extrch .eq. 'MOY') then
        imar = 1
        do 55 ipi = 1, npi
            do 56 ispi = 1, nspi
                do 57 ivalcp = 1, nvalcp
                    val2r = zr(&
                            jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*nbspi*(ivalcp-1) +nbspi*(ipir-1&
                            &) +(ispir-1)&
                            )
                    zr(jelga+nbcmp*nbpi*nbspi*(imar-1) +nbpi*nbspi*(&
                    ivalcp-1) +nbspi*(ipir-1) +(ispir-1)) = val2r/&
                    nbma
57              continue
56          continue
55      continue
    endif
!
    call jedema()
!
end subroutine
