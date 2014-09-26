subroutine nmdopi(modelz, numedd, method, lreli, sdpilo)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/nmcoef.h"
#include "asterfort/nmdire.h"
#include "asterfort/nmmein.h"
#include "asterfort/nueqch.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modelz
    character(len=24) :: numedd
    character(len=16) :: method(*)
    character(len=19) :: sdpilo
    aster_logical :: lreli
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNEES)
!
! CONSTRUCTION DE LA SD PILOTAGE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  METHOD : DESCRIPTION DE LA METHODE DE RESOLUTION
! IN  LRELI  : .TRUE. SI RECHERCHE LINEAIRE
! OUT SDPILO : SD PILOTAGE
!               .PLTK
!                (1) = TYPE DE PILOTAGE
!                (2) = LIGREL POUR LES PILOTAGES PAR ELEMENTS
!                (3) = NOM DE LA CARTE DU TYPE (PILO_K)
!                (4) = NOM DE LA CARTE DU TYPE (PILO_R) MIN/MAX
!                (5) = PROJECTION 'OUI' OU 'NON' SUR LES BORNES
!                (6) = TYPE DE SELECTION : 'RESIDU',
!                        'NORM_INCR_DEPL' OU 'ANGL_INCR_DEPL'
!                (7) = EVOLUTION DES BORNES
!                        'CROISSANT', 'DECROISSANT' OU 'SANS'
!               .PLCR  COEFFICIENTS DU PILOTAGE
!               .PLCI  REPERAGE DES BINOMES (ARETE,COMPOSANTE) AVEC XFEM
!               .PLIR  PARAMETRES DU PILOTAGE
!                (1) = COEF_PILO
!                (2) = ETA_PILO_MAX
!                (3) = ETA_PILO_MIN
!                (4) = ETA_PILO_R_MAX
!                (5) = ETA_PILO_R_MIN
!                (6) = COEF_PILO AU PAS DE TEMPS CONVERGE PRECEDENT
!
!
!
!
    integer :: nbno, numequ, nddl, nb_node_mesh
    integer :: nume_node, nume_node_1, nume_node_2, numequ_1, numequ_2
    integer :: ino, iddl
    integer :: jvale
    integer :: jplir, jpltk
    integer :: ibid, n1, n2, neq, ndim
    real(kind=8) :: coef, lm(2)
    character(len=8) :: mesh, lborn(2), nomcmp
    character(len=8) :: modele, fiss
    character(len=16) :: relmet
    character(len=24) :: lisnoe, liscmp
    integer :: jlinoe, jlicmp
    character(len=24) :: typpil, projbo, typsel, evolpa, txt(2)
    character(len=19) :: chapil, selpil, ligrmo, ligrpi
    character(len=19) :: careta, cartyp, chapic
    real(kind=8) :: etrmax, etrmin, etamin, etamax
    integer :: nbmocl
    character(len=16) :: limocl(2), tymocl(2)
    integer :: ifm, niv
    integer :: jlino1, jlino2, nbnom
    integer :: jeq2, ierm
    character(len=8) :: compo
    character(len=19) :: grln, cnsln, grlt
    character(len=24) :: liseq2, lisno1, lisno2
    real(kind=8) :: coef1, coef2, coefi, vect(3)
    aster_logical :: isxfe, selxfe, selfem
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: plsl(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    modele = modelz
    call exixfe(modele, ierm)
    isxfe=(ierm.eq.1)
    call dismoi('NOM_MAILLA', numedd, 'NUME_DDL', repk=mesh)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
    call dismoi('DIM_GEOM', mesh, 'MAILLAGE', repi=ndim)
    lisnoe = '&&NMDOPI.LISNOE'
    liscmp = '&&NMDOPI.LISCMP'
    nbmocl = 2
    limocl(1) = 'GROUP_NO'
    limocl(2) = 'NOEUD'
    tymocl(1) = 'GROUP_NO'
    tymocl(2) = 'NOEUD'
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD PILOTAGE'
    endif
!
! --- LECTURE DU TYPE ET DE LA ZONE
!
    call wkvect(sdpilo(1:19)// '.PLTK', 'V V K24', 7, jpltk)
    call getvtx('PILOTAGE', 'TYPE', iocc=1, scal=typpil, nbret=n1)
    zk24(jpltk) = typpil
    call getvtx('PILOTAGE', 'PROJ_BORNES', iocc=1, scal=projbo, nbret=n1)
    zk24(jpltk+4) = projbo
    call getvtx('PILOTAGE', 'SELECTION', iocc=1, scal=typsel, nbret=n1)
    zk24(jpltk+5) = typsel
    call getvtx('PILOTAGE', 'EVOL_PARA', iocc=1, scal=evolpa, nbret=n1)
    zk24(jpltk+6) = evolpa
!
! --- PARAMETRES COEF_MULT ET ETA_PILO_MAX
!
    call wkvect(sdpilo(1:19)// '.PLIR', 'V V R8', 6, jplir)
    call getvr8('PILOTAGE', 'COEF_MULT', iocc=1, scal=coef, nbret=n1)
    zr(jplir) = coef
    zr(jplir+5) = coef
    if (abs(coef) .le. r8prem()) then
        call utmess('F', 'PILOTAGE_3')
    endif
!
    call getvr8('PILOTAGE', 'ETA_PILO_R_MAX', iocc=1, scal=etrmax, nbret=n1)
    if (n1 .ne. 1) etrmax = r8gaem()
    zr(jplir+3) = etrmax
!
    call getvr8('PILOTAGE', 'ETA_PILO_R_MIN', iocc=1, scal=etrmin, nbret=n2)
    if (n2 .ne. 1) etrmin = -r8gaem()
    zr(jplir+4) = etrmin
!
    call getvr8('PILOTAGE', 'ETA_PILO_MAX', iocc=1, scal=etamax, nbret=n1)
    if (n1 .ne. 1) then
        etamax = r8vide()
    else
        if (etamax .gt. zr(jplir+3)) then
            call utmess('F', 'PILOTAGE_48')
        endif
    endif
    zr(jplir+1) = etamax
!
    call getvr8('PILOTAGE', 'ETA_PILO_MIN', iocc=1, scal=etamin, nbret=n2)
    if (n2 .ne. 1) then
        etamin = r8vide()
    else
        if (etamin .lt. zr(jplir+4)) then
            call utmess('F', 'PILOTAGE_49')
        endif
    endif
    zr(jplir+2) = etamin
!
    if (typpil .eq. 'SAUT_IMPO' .or. typpil .eq. 'SAUT_LONG_ARC') then
        if (.not.isxfe) then
            call utmess('F', 'PILOTAGE_60')
        endif
        call getvid('PILOTAGE', 'FISSURE', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvid('PILOTAGE', 'FISSURE', iocc=1, scal=fiss, nbret=n1)
        else
            call utmess('F', 'PILOTAGE_58')
        endif
    endif
!
    if (isxfe .and. (typsel.eq.'ANGL_INCR_DEPL' .or.typsel.eq.'NORM_INCR_DEPL')) then
        call getvid('PILOTAGE', 'FISSURE', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvid('PILOTAGE', 'FISSURE', iocc=1, scal=fiss, nbret=n1)
        else
            call utmess('F', 'PILOTAGE_59')
        endif
    endif
! ======================================================================
!             PILOTAGE PAR PREDICTION ELASTIQUE : PRED_ELAS
! ======================================================================
!
    if (typpil .eq. 'PRED_ELAS' .or. typpil .eq. 'DEFORMATION') then
!
        call exlima('PILOTAGE', 1, 'V', modele, ligrpi)
        zk24(jpltk+1) = ligrpi
!
!
        cartyp = '&&NMDOPI.TYPEPILO'
        ligrmo = modele // '.MODELE'
        call mecact('V', cartyp, 'MODELE', ligrmo, 'PILO_K',&
                    ncmp=1, nomcmp='TYPE', sk=typpil)
        zk24(jpltk+2) = cartyp
!
        lm(1) = etrmax
        lm(2) = etrmin
        careta = '&&NMDOPI.BORNEPILO'
        lborn(1) = 'A0'
        lborn(2) = 'A1'
        call mecact('V', careta, 'MODELE', ligrmo, 'PILO_R',&
                    ncmp=2, lnomcmp=lborn, vr=lm)
        zk24(jpltk+3) = careta
!
!
!
! ======================================================================
!              PILOTAGE PAR UN DEGRE DE LIBERTE : DDL_IMPO
! ======================================================================
!
    else if (typpil .eq. 'DDL_IMPO'.or.typpil .eq. 'SAUT_IMPO') then
!
        call reliem(modele, mesh, 'NU_NOEUD', 'PILOTAGE', 1,&
                    nbmocl, limocl, tymocl, lisnoe, nbno)
        if (typpil .eq. 'DDL_IMPO') then
            if (nbno .ne. 1) then
                call utmess('F', 'PILOTAGE_50')
            endif
            coef = 1.d0
        endif
!
!
! ======================================================================
!      PILOTAGE PAR UNE METHODE DE TYPE LONGUEUR D'ARC : LONG_ARC
! ======================================================================
!
    else if (typpil.eq.'LONG_ARC'.or.typpil.eq.'SAUT_LONG_ARC') then
!
        call reliem(modele, mesh, 'NU_NOEUD', 'PILOTAGE', 1,&
                    nbmocl, limocl, tymocl, lisnoe, nbno)
        if (typpil .eq. 'LONG_ARC') then
            if (nbno .eq. 0) then
                call utmess('F', 'PILOTAGE_57')
            endif
            coef = 1.d0 / nbno
        endif
    endif
!
! --- CREATION SD SELECTION DES DDLS EN FEM ?
!
    selfem = ((typpil .eq. 'LONG_ARC' ).or.(typpil .eq. 'DDL_IMPO' ))
!
! --- CREATION SD SELECTION DES DDLS EN X-FEM ?
!
    selxfe = (&
             (typpil.eq.'SAUT_LONG_ARC') .or. (typpil .eq. 'SAUT_IMPO') .or.&
             (isxfe.and.typsel.ne.'RESIDU')&
             )
!
    if (selfem) then
        call getvtx('PILOTAGE', 'NOM_CMP', iocc=1, nbval=0, nbret=nddl)
        nddl = -nddl
        if (nddl .ne. 1 .and. typpil .eq. 'DDL_IMPO') then
            txt(1)='NOM_CMP'
            txt(2)=typpil
            call utmess('F', 'PILOTAGE_56', nk=2, valk=txt)
        else if (nddl.eq.0.and.typpil.eq.'LONG_ARC') then
            txt(1)='NOM_CMP'
            txt(2)=typpil
            call utmess('F', 'PILOTAGE_55', nk=2, valk=txt)
        endif
        if (nddl .gt. 0) then
            call wkvect(liscmp, 'V V K8', nddl, jlicmp)
            call getvtx('PILOTAGE', 'NOM_CMP', iocc=1, nbval=nddl, vect=zk8(jlicmp),&
                        nbret=ibid)
        endif
        call jeveuo(lisnoe, 'L', jlinoe)
    endif
!
!
!
    if (selxfe) then
        call getvtx('PILOTAGE', 'DIRE_PILO', iocc=1, nbval=0, nbret=nddl)
        nddl = -nddl
        if (nddl .ne. 1 .and. typpil .eq. 'SAUT_IMPO') then
            txt(1)='DIRE_PILO'
            txt(2)=typpil
            call utmess('F', 'PILOTAGE_56', nk=2, valk=txt)
        else if (nddl.eq.0.and.typpil.eq.'SAUT_LONG_ARC') then
            txt(1)='DIRE_PILO'
            txt(2)=typpil
            call utmess('F', 'PILOTAGE_55', nk=2, valk=txt)
        else if (nddl.eq.0) then
            call utmess('F', 'PILOTAGE_64', sk=typsel)
        endif
        if (nddl .gt. 0) then
            call wkvect(liscmp, 'V V K8', nddl, jlicmp)
            call getvtx('PILOTAGE', 'DIRE_PILO', iocc=1, nbval=nddl, vect=zk8( jlicmp),&
                        nbret=ibid)
        endif
!
        lisno1 ='&&NMDOPI.LISNO1'
        lisno2 ='&&NMDOPI.LISNO2'
        cnsln ='&&NMDOPI.CNSLN'
        grln ='&&NMDOPI.GRLN'
        grlt ='&&NMDOPI.GRLT'
        call cnocns(fiss//'.LNNO', 'V', cnsln)
        call cnocns(fiss//'.GRLNNO', 'V', grln)
        call cnocns(fiss//'.GRLTNO', 'V', grlt)
!
        call nmmein(fiss, mesh, nbno, lisnoe, liscmp,&
                    nbnom, lisno1, lisno2, ndim, compo)
        call jeveuo(lisno1, 'L', jlino1)
        call jeveuo(lisno2, 'L', jlino2)
        nbno=nbnom
        liseq2='&&NMDOPI.LISEQ2'
        call wkvect(liseq2, 'V V I', nbno, jeq2)
        chapic = sdpilo(1:14)//'.PLCI'
        call vtcreb(chapic, 'V', 'R', nume_ddlz = numedd, nb_equa_outz = neq)
        call jeveuo(chapic(1:19)//'.VALE', 'E', vr=vale)
    endif
!
    if (selfem .or. selxfe) then
        chapil = sdpilo(1:14)//'.PLCR'
        call vtcreb(chapil, 'V', 'R', nume_ddlz = numedd, nb_equa_outz = neq)
        call jeveuo(chapil(1:19)//'.VALE', 'E', jvale)
        call jeveuo(liscmp, 'L', jlicmp)
        call jelira(liscmp, 'LONMAX', ival=nddl)
!
        do iddl = 1, nddl
            nomcmp = zk8(jlicmp-1+iddl) 
            do ino = 1, nbno
                if (selxfe) then
                    nume_node_1 = zi(jlino1-1+ino)
                    nume_node_2 = zi(jlino2-1+ino)
                    call nueqch('F', chapil, nume_node_1, nomcmp, numequ_1)
                    call nueqch('F', chapil, nume_node_2, nomcmp, numequ_2)
                else if (selfem) then
                    nume_node = zi(jlinoe-1+ino)
                    call nueqch('F', chapil, nume_node, nomcmp, numequ)
                endif

                if (selxfe) then
                    if (compo(1:4) .eq. 'DTAN' .or. compo .eq. 'DNOR') then
                        call nmdire(nume_node_1, nume_node_2, ndim, cnsln, grln,&
                                    grlt, compo, vect)
                    endif
                    call nmcoef(nume_node_1, nume_node_2, typpil, nbno, cnsln,&
                                compo, vect, iddl, ino, coef1,&
                                coef2, coefi)
                    zr(jvale-1+numequ_1) = coef1
                    vale(numequ_1) = coefi
                    zr(jvale-1+numequ_2) = coef2
                    vale(numequ_2) = coefi
                else if (selfem) then
                    zr(jvale-1+numequ) = coef
                endif
            end do
        end do
    endif
!
    call jedetr(lisnoe)
    call jedetr(liscmp)
    if (selxfe) then
        call jedetr(lisno1)
        call jedetr(lisno2)
        call jedetr(cnsln)
        call jedetr(grln)
    endif
!
! --- CREATION SD REPERAGE DES DX/DY/DZ
!
    if (typpil .eq. 'LONG_ARC') then
        selpil = sdpilo(1:14)//'.PLSL'
        call vtcreb(selpil, 'V', 'R', nume_ddlz = numedd, nb_equa_outz = neq)
        call jeveuo(selpil(1:19)//'.VALE', 'E', vr=plsl)
        nddl = 3
        call wkvect(liscmp, 'V V K8', nddl, jlicmp)
        zk8(jlicmp-1+1) = 'DX'
        zk8(jlicmp-1+2) = 'DY'
        zk8(jlicmp-1+3) = 'DZ'
        do iddl = 1, nddl
            nomcmp = zk8(jlicmp-1+iddl) 
            do ino = 1, nb_node_mesh
                nume_node = ino
                call nueqch('I', selpil, nume_node, nomcmp, numequ)
                if (numequ .ne. 0) then
                    plsl(numequ) = 1.d0
                endif
            end do
        end do
    endif
!
! --- GESTION RECHERCHE LINEAIRE
!
    if (lreli) then
        relmet = method(7)
        if (typpil .ne. 'DDL_IMPO') then
            if (relmet .ne. 'PILOTAGE') then
                call utmess('F', 'PILOTAGE_4')
            endif
        endif
    endif
!
    call jedetr(lisnoe)
    call jedetr(liscmp)
    call jedema()
end subroutine
