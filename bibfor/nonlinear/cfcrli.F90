subroutine cfcrli(noma, numedd, defico, resoco)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfecrd.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnomm.h"
#include "asterfort/cfverd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/posddl.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - LIAISONS LINEAIRES)
!
! CREATION DES SD POUR LES LIAISONS LINEAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMEDD : NOM DU NUME_DDL
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: nnoco, ndimg, ntpc
    character(len=24) :: apcoef, apcofr
    integer :: japcoe, japcof
    character(len=24) :: ddlco, nbddl, apddl, approj
    integer :: jddl, jnbddl, japddl, jappro
    character(len=24) :: coco
    integer :: jcoco
    character(len=24) :: appoin, numlia
    integer :: japptr, jnumli
    character(len=19) :: statfr, liac, liot, typl
    integer :: jstfr, jliac, jliot, jtypl
    integer :: numddl, numnoe
    integer :: ino, iddl, posno
    integer :: iret
    character(len=8) :: nomnoe, k8bid
    integer :: neq, nesmax, nddl
    logical :: lctfd, llagrf
    integer :: zcoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CREATION DE LA SD POUR LES '//&
        ' LIAISONS LINEAIRES'
    endif
!
! --- INITIALISATIONS
!
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(defico,'FROT_DISCRET')
    llagrf = cfdisl(defico,'FROT_LAGR')
!
! --- INFORMATIONS
!
    nnoco = cfdisi(defico,'NNOCO' )
    ndimg = cfdisi(defico,'NDIM' )
    ntpc = cfdisi(defico,'NTPC' )
    nesmax = ntpc
!
! --- VERIFICATION DE LA COHERENCE DES DIMENSIONS (PUR 2D OU PUR 3D)
!
    if (ndimg .eq. 2) then
        call cfverd(noma, numedd, defico)
    endif
!
! --- NOMBRE DE DDLS PAR NOEUD
!
    nbddl = resoco(1:14)//'.NBDDL'
    call wkvect(nbddl, 'V V I', nnoco+1, jnbddl)
    zi(jnbddl) = 0
    nddl = 0
    do 20 ino = 1, nnoco
        nddl = nddl + ndimg
        zi(jnbddl+ino) = nddl
20  end do
!
! --- NUMEROS DES DDL
!
    ddlco = resoco(1:14)//'.DDLCO'
    call wkvect(ddlco, 'V V I', nddl, jddl)
!
    do 25 ino = 1, nnoco
        posno = ino
        call cfnomm(noma, defico, 'NOEU', posno, nomnoe)
!
        iddl = zi(jnbddl+ino-1) + 1
!
        call posddl('NUME_DDL', numedd, nomnoe, 'DX', numnoe,&
                    numddl)
!
        if (numddl .eq. 0) then
            ASSERT(.false.)
        else
            zi(jddl+iddl-1) = numddl
        endif
!
        call posddl('NUME_DDL', numedd, nomnoe, 'DY', numnoe,&
                    numddl)
!
        if (numddl .eq. 0) then
            ASSERT(.false.)
        else
            zi(jddl+iddl) = numddl
        endif
!
        if (ndimg .eq. 3) then
            call posddl('NUME_DDL', numedd, nomnoe, 'DZ', numnoe,&
                        numddl)
!
            if (numddl .eq. 0) then
                ASSERT(.false.)
            else
                zi(jddl+iddl+1) = numddl
            endif
        endif
!
25  end do
!
! --- TABLEAU DES POINTEURS POUR LES DDLS
!
    apddl = resoco(1:14)//'.APDDL'
    call wkvect(apddl, 'V V I', 30*ntpc, japddl)
!
! --- NOMBRE DE DDL PAR LIAISON
!
    appoin = resoco(1:14)//'.APPOIN'
    call wkvect(appoin, 'V V I', ntpc+1, japptr)
!
! --- NUMERO DU POINT ET DU NOEUD ESCLAVE POUR LA LIAISON
!
    numlia = resoco(1:14)//'.NUMLIA'
    call wkvect(numlia, 'V V I', 4*ntpc, jnumli)
!
! --- VECTEUR POUR LA GESTION DES LIAISONS (CFDISD/CFECRD)
!
    coco = resoco(1:14)//'.COCO'
    zcoco = cfmmvd('ZCOCO')
    call wkvect(coco, 'V V I', zcoco, jcoco)
    call cfecrd(resoco, 'NDIM', ndimg)
    call cfecrd(resoco, 'NEQ', neq)
    call cfecrd(resoco, 'NESMAX', nesmax)
!
! --- TYPES DE LIAISON
! ---        VAUT C0 : LIAISON DE CONTACT
! ---        VAUT F0 : LIAISON DE FROTTEMENT ADHERENT (DEUX DIRECTIONS)
! ---        VAUT F1 : LIAISON DE FROTTEMENT ADHERENT (1ERE DIRECTION )
! ---        VAUT F2 : LIAISON DE FROTTEMENT ADHERENT (2EME DIRECTION )
!
    typl = resoco(1:14)//'.TYPL'
    call wkvect(typl, 'V V K8', 2*ntpc, jtypl)
!
! --- OBJET DE SAUVEGARDE DE L'ETAT DES LIAISONS DE FROTTEMENT
! --- EN LAGRANGIEN
!
    if (llagrf) then
!       ETAT CONVERGE
        statfr = resoco(1:14)//'.STF0'
        call wkvect(statfr, 'V V K8', nnoco, jstfr)
!       ETAT COURANT AVANT APPARIEMENT
        statfr = resoco(1:14)//'.STFR'
        call wkvect(statfr, 'V V K8', nnoco, jstfr)
    endif
!
! --- LIAISONS OTEES (PIVOTS NULS)
!
    liot = resoco(1:14)//'.LIOT'
    call wkvect(liot, 'V V I', 4*ntpc+4, jliot)
!
! --- LIAISONS ACTIVES
!
    liac = resoco(1:14)//'.LIAC'
    call wkvect(liac, 'V V I', 3*ntpc+1, jliac)
!
! --- COEFFICIENTS DES RELATIONS LINEAIRES
!
    apcoef = resoco(1:14)//'.APCOEF'
    call wkvect(apcoef, 'V V R', 30*ntpc, japcoe)
    if (lctfd) then
        apcofr = resoco(1:14)//'.APCOFR'
        call wkvect(apcofr, 'V V R', 60*ntpc, japcof)
    endif
!
! --- COORDONNEES DE LA PROJECTION
!
    approj = resoco(1:14)//'.APPROJ'
    call wkvect(approj, 'V V R', 3*ntpc, jappro)
!
    call jedema()
end subroutine
