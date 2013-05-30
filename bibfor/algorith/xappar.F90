subroutine xappar(loptin, noma, modele, defico, resoco)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mmgaus.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/xcopco.h'
    include 'asterfort/xmcoor.h'
    include 'asterfort/xmrema.h'
    include 'asterfort/xmrept.h'
    include 'asterfort/xmrlst.h'
    include 'asterfort/xpivit.h'
    include 'asterfort/xxmmvd.h'
    logical :: loptin
    character(len=8) :: noma, modele
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! REALISE L'APPARIEMENT ENTRE SURFACE ESCLAVE ET SURFACE MAITRE POUR
! LE CONTACT METHODE CONTINUE.
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! ----------------------------------------------------------------------
!
!
! METHODE : POUR CHAQUE POINT DE CONTACT (SUR UNE MAILLE ESCLAVE ET
! AVEC UN SCHEMA D'INTEGRATION DONNE), ON RECHERCHE LE NOEUD MAITRE LE
! PLUS PROCHE ET ON PROJETTE SUR LES MAILLES QUI L'ENTOURE
!
! STOCKAGE DES POINTS  DE CONTACT DES SURFACES  ESCLAVES ET APPARIEMENT
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: zmesx, ztabf, zxain
    integer :: ifm, niv
    integer :: i, ipc, tyco
    integer :: ndim, ntmae, ntpc, nbpc
    integer :: iface, imae, izone, ifamin
    integer :: jcesd(10), jcesv(10), jcesl(10), iad
    integer :: mmait, amait, nmait, group, statue, stamin
    integer :: nummae, nummin
    integer :: npte, nface, nvit, naret
    real(kind=8) :: geom(3), ksipc1, ksipc2, wpc
    real(kind=8) :: t1min(3), t2min(3), ximin, yimin
    real(kind=8) :: jeumin, coor(3), norm(3), noor
    real(kind=8) :: rre, rrm
    character(len=8) :: alias
    character(len=19) :: chs(7)
    character(len=24) :: xfimai, cncte
    character(len=24) :: tabfin, maescx
    integer :: jtabf, jmaesx, ninter
    logical :: projin, lcinit, lgliss
    integer :: jxc, jfimai, ifiss, ifism, ipc2, numpi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM> ... APPARIEMENT'
    endif
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    tabfin = resoco(1:14)//'.TABFIN'
    maescx = defico(1:16)//'.MAESCX'
    xfimai = defico(1:16)//'.XFIMAI'
    call jeveuo(tabfin, 'E', jtabf)
    call jeveuo(maescx, 'L', jmaesx)
    call jeveuo(xfimai, 'L', jfimai)
!
    ztabf = cfmmvd('ZTABF')
    zmesx = cfmmvd('ZMESX')
    zxain = xxmmvd('ZXAIN')
!
! --- INITIALISATIONS
!
    chs(1) = '&&XAPPAR.CHSLO'
    chs(2) = '&&XAPPAR.CHSAI'
    chs(3) = '&&XAPPAR.CHSPI'
    chs(4) = '&&XAPPAR.CHSCF'
    chs(5) = '&&XAPPAR.CHSGE'
    chs(6) = '&&XAPPAR.CHSGM'
    chs(7) = '&&XAPPAR.CHSLT'
    ntpc = 0
    do 9 i = 1, 3
        geom(i) = 0.d0
        t1min(i) = 0.d0
        t2min(i) = 0.d0
 9  end do
    ndim = cfdisi(defico,'NDIM' )
    ntmae = cfdisi(defico,'NTMAE')
!
! --- ON RECUPERE RESPECTIVEMENT :
! --- LES CHAMPS DES GEOMETRIE ESCLAVE ET MAITRE
! --- LES INFOS SUR LE NOMBRES DE PT D'INTER ET LE NBRE DE FACETTE
! --- LES INFOS SUR LES ARETES COUPEES
! --- LES NUMERO LOCAUX DES NOEUDS DES FACETTES DE CONTACT
! --- LE CHAMP NOEUD DE LA LST
!
    call celces(modele//'.TOPOFAC.LO', 'V', chs(1))
    call celces(modele//'.TOPOFAC.AI', 'V', chs(2))
    call celces(modele//'.TOPOFAC.PI', 'V', chs(3))
    call celces(modele//'.TOPOFAC.CF', 'V', chs(4))
    call celces(modele//'.TOPOFAC.GE', 'V', chs(5))
    call celces(modele//'.TOPOFAC.GM', 'V', chs(6))
    call celces(modele//'.LTNO', 'V', chs(7))
!
    do 10 i = 1, 7
        call jeveuo(chs(i)//'.CESD', 'L', jcesd(i))
        call jeveuo(chs(i)//'.CESV', 'L', jcesv(i))
        call jeveuo(chs(i)//'.CESL', 'L', jcesl(i))
10  end do
!
! --- BOUCLE SUR LES MAILLES ESCLAVES
!
    do 100 imae = 1, ntmae
!
! --- ZONE DE CONTACT ET FISSURE LOCALE ASSOCIÉE
!
        izone = zi(jmaesx+zmesx*(imae-1)+2-1)
        ifiss = zi(jmaesx+zmesx*(imae-1)+5-1)
!
! --- OPTIONS SUR LA ZONE DE CONTACT
!
        tyco = mminfi(defico,'INTEGRATION' ,izone )
        lgliss = mminfl(defico,'GLISSIERE_ZONE',izone )
        lcinit = (mminfi(defico,'CONTACT_INIT' ,izone ).eq.1)
        cncte = zk8(jfimai-1+izone)//'.CNCTE'
!
! --- INFOS SUR LA MAILLE ESCLAVE COURANTE
!
        nummae = zi(jmaesx+zmesx*(imae-1)+1-1)
        nbpc = zi(jmaesx+zmesx*(imae-1)+3-1)
        statue = zi(jmaesx+zmesx*(imae-1)+4-1)
!
        call jeveuo(modele//'.XFEM_CONT', 'L', jxc)
        if (ndim .eq. 2) then
            if (zi(jxc) .le. 2) alias='SE2'
            if (zi(jxc) .eq. 3) alias='SE3'
        else if (ndim.eq.3) then
            alias='TR3'
        endif
!
! --- ON RECUPERE LE NOMBRE DE POINTS D'INTERSECTION
! --- DE LA MAILLE ESCLAVE
!
        call cesexi('C', jcesd(1), jcesl(1), nummae, 1,&
                    ifiss, 1, iad)
        call assert(iad.gt.0)
        ninter = zi(jcesv(1)-1+iad)
!
! --- ON RECUPERE LE NOMBRE DE POINTS PAR FACETTE
! --- DE LA MAILLE ESCLAVE
        call cesexi('C', jcesd(1), jcesl(1), nummae, 1,&
                    ifiss, 3, iad)
        call assert(iad.gt.0)
        npte = zi(jcesv(1)-1+iad)
!
! --- ON RECUPERE LE NOMBRE DE FACETTES DE CONTACT DE LA MAILLE ESCLAVE
!
        call cesexi('C', jcesd(1), jcesl(1), nummae, 1,&
                    ifiss, 2, iad)
        call assert(iad.gt.0)
        nface = max(1,zi(jcesv(1)-1+iad))
!
        if (nbpc .eq. 0) goto 100
!
! --- BOUCLE SUR LES FACETTES DE CONTACT
!
        do 105 iface = 1, nface
!
! --- APPARIEMENT - BOUCLE SUR LES POINTS DE CONTACT
!
            do 110 ipc = 1, nbpc
!
! --- COORDONNEES DANS ELEMENT DE REFERENCE ET POIDS DU POINT DE CONTACT
!
! --- FAUX POINT D'INTEGRATION (POUR L'ÉLIMINATION DES DDL EN TROP)
                if (statue .lt. 0) then
                    do 120 ipc2 = 1, npte
                        call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                                    ifiss, ipc2, iad)
                        numpi = zi(jcesv(4)-1+iad)
                        call cesexi('S', jcesd(2), jcesl(2), nummae, 1,&
                                    ifiss, zxain*(numpi-1)+1, iad)
                        if (zr(jcesv(2)-1+iad) .ne. 0) goto 130
                        call cesexi('S', jcesd(2), jcesl(2), nummae, 1,&
                                    ifiss, zxain*(numpi-1)+2, iad)
                        if (zr(jcesv(2)-1+iad) .ne. 0) goto 130
120                  continue
                    call assert(.false.)
130                  continue
                    call mmgaus(alias, tyco, ipc2, ksipc1, ksipc2,&
                                wpc)
                else
                    call mmgaus(alias, tyco, ipc, ksipc1, ksipc2,&
                                wpc)
                endif
!
! --- CALCUL DES COORDONNEES REELLES DU POINT DE CONTACT
!
                call xcopco(jcesd, jcesv, jcesl, ifiss, alias,&
                            ndim, nummae, iface, ksipc1, ksipc2,&
                            npte, geom)
!
! --- RECHERCHE DU PT D'INTERSECTION SUR LE COTE MAÎTRE LE PLUS PROCHE
! --- DU POINT DE CONTACT
!
                if (statue .gt. 0 .and. statue .ne. 2) then
                    call xmrept(jcesd, jcesv, jcesl, izone, ndim,&
                                defico, geom, statue, mmait, amait,&
                                nmait)
                endif
!
! --- PROJECTION DU PT DE CONTACT SUR LA FACETTE DE CONTACT
! --- LA PLUS PROCHE
!
                call xmrema(jcesd, jcesv, jcesl, noma, ndim,&
                            ifiss, defico, izone, alias, mmait,&
                            amait, nmait, statue, geom, nummin,&
                            nummae, ifamin, iface, jeumin, t1min,&
                            t2min, ximin, yimin, projin, stamin,&
                            ifism)
!
! --- NUMEROS DES MAILLE ESCLAVE ET MAITRE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+1) = nummae
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+2) = nummin
!
! --- COORDONNEES GEOMETRIQUES DU POINT DE CONTACT
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+3) = ksipc1
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+12) = ksipc2
!
! --- VECTEURS TANGENTS DANS LE POINT DE CONTACT
!
! --- EN 3D, ON ORIENTE LES TANGENTES
!
                if (ndim .eq. 3) then
! --- ON CALCULE LA NORMALE
                    call provec(t1min, t2min, norm)
                    call normev(norm, noor)
! --- ON PROJETE LA DIRECTION X SUR SUR LE PLAN TANGENT,
! --- POUR OBTENIR LA PREMIERE DIRECTION TANGENTE
                    if (abs(norm(1)) .ne. 1) then
                        t1min(1) = 1-norm(1)**2
                        t1min(2) = -norm(1)*norm(2)
                        t1min(3) = -norm(1)*norm(3)
                    else
                        t1min(1) = -norm(2)*norm(1)
                        t1min(2) = 1-norm(2)**2
                        t1min(3) = -norm(2)*norm(3)
                    endif
! --- DEUXIÈME DIRECTION TANGENTE
                    call provec(norm, t1min, t2min)
!
! --- NORMALISATION DES VECTEURS TANGENTS
!
                    call normev(t1min, noor)
                    call normev(t2min, noor)
!
                endif
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+6) = t1min(1)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+7) = t1min(2)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+8) = t1min(3)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+9) = t2min(1)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+10) = t2min(2)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+11) = t2min(3)
!
! --- NUMERO DE LA ZONE DE FISSURE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+15) = izone
!
! --- POIDS DU POINT DU CONTACT
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+16) = wpc
!
! --- NOMBRE DE POINTS D'INTERSECTIONS ESCLAVE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+24) = npte
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+14) = ninter
!
! --- NUMERO DE LA FACETTE ESCLAVE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+25) = iface
!
! --- NOMBRE DE FACETTES ESCLAVES
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+26) = nface
!
! --- COORDONNEES DANS L'ELEMENT PARENT MAITRE
!
                call xmcoor(jcesd, jcesv, jcesl, ifism, ndim,&
                            npte, nummin, ifamin, ximin, yimin,&
                            coor)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+20) = coor(1)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+21) = coor(2)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+23) = coor(3)
!
! --- STATUT DE LA MAILLE MAITRE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+32) = stamin
!
! --- CALCUL DE SQRT LST DU PT DE CONTACT MAITRE
!
                call xmrlst(jcesd, jcesv, jcesl, noma, nummin,&
                            coor, rrm)
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+31) = rrm
!
! --- NUMÉRO LOCALE DE FISSURE MAITRE
!
                zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+34) = ifism
!
                if (loptin) then
!
! --- NUMÉRO LOCALE DE FISSURE ESCLAVE
!
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+33) = ifiss
!
! --- COORDONNEES DANS L'ELEMENT PARENT ESCLAVE
!
                    call xmcoor(jcesd, jcesv, jcesl, ifiss, ndim,&
                                npte, nummae, iface, ksipc1, ksipc2,&
                                coor)
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+17) = coor(1)
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+18) = coor(2)
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+19) = coor(3)
!
! --- CALCUL DE SQRT LST DU PT DE CONTACT ESCLAVE
!
                    call xmrlst(jcesd, jcesv, jcesl, noma, nummae,&
                                coor, rre)
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+30) = rre
!
! --- POINT D'INTEGRATION VITAL OU PAS
! --- NUMERO DE GROUPE ET D'ARETE (SI LE PT EST SUR UNE ARETE CONNECTÉE)
!
!
                    if (mod(tyco,10) .eq. 2) then
!---- CAS DES SCHEMAS DE GAUSS OU LE PT N'EST PAS SUR UNE ARETE
                        nvit = 1
                        group = 0
                        naret = 0
                    else
                        call xpivit(jcesd, jcesv, jcesl, ifiss, cncte,&
                                    ndim, nummae, iface, ksipc1, ksipc2,&
                                    nvit, group, naret)
                        if (statue .lt. 0) nvit=0
                    endif
!
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+27) = nvit
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+4) = group
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+5) = naret
!
! --- CONTACT_INIT (13) ET MEMOIRE DE CONTACT (28) LA MEMO DE CONTACT
! --- EST INITIALISEE AVEC CONTACT_INI ET SERT POUR LE CONTACT GLISSIERE
!
                    if (lcinit) then
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+13) = 1.d0
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+28) = 1.d0
                    else
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+13) = 0.d0
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+28) = 0.d0
                    endif
!
! --- ON RENSEIGNE LE CONTACT GLISSIERE SI DECLARE
!
                    if (lgliss) then
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+29) = 1.d0
                    else
                        zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+29) = 0.d0
                    endif
                endif
!
! --- NOEUDS EXCLUS PAR PROJECTION HORS ZONE
!
                if (.not. projin) then
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+22) = 1.d0
                else
                    zr(jtabf+ztabf*ntpc+ztabf*(ipc-1)+22) = 0.d0
                endif
110          continue
            ntpc = ntpc + nbpc
105      continue
100  end do
    zr(jtabf-1+1) = ntpc
    call assert(ntpc.eq.cfdisi(defico, 'NTPC'))
!
! --- MENAGE
!
    do 200 i = 1, 7
        call detrsd('CHAM_ELEM_S', chs(i))
200  end do
!
    call jedema()
end subroutine
